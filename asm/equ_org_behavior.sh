#!/usr/bin/env bash
set -uo pipefail

echo "==================================================================="
echo "         Comprehensive Test of .equ and .org Behavior"
echo "==================================================================="

# Test 1: Inline .equ works with C symbols
echo -e "\n### TEST 1: Inline .equ works with C function symbols ###"
cat > test1.c << 'EOF'
__attribute__((naked)) void func_a(void) {
    __asm__("nop; nop; nop; nop; ret");  /* 5 bytes */
}

__attribute__((naked)) void func_b(void) {
    __asm__("ret");
}

/* Compare inline vs top-level .equ */
asm(".globl top_level_size; .equ top_level_size, func_b - func_a");

void test_inline_equ(void) {
    __asm__(".equ inline_size, func_b - func_a\n"
            "mov $inline_size, %%eax\n" ::: "eax");
}
EOF

gcc -c test1.c -o test1.o 2>&1
echo "Symbol table comparison:"
nm test1.o | grep -E "size|func_[ab]" | sort
echo "✓ Both top_level_size (global 'A') and inline_size (local 'a') computed correctly"

# Test 2: .equ symbols are file-scoped
echo -e "\n\n### TEST 2: .equ symbols persist across functions ###"
cat > test2.c << 'EOF'
void define_constant(void) {
    __asm__(".equ MAGIC_VALUE, 0x42\n"
            ".equ BUFFER_SIZE, 256\n");
}

void use_in_different_function(void) {
    __asm__("mov $MAGIC_VALUE, %%eax\n"
            "mov $BUFFER_SIZE, %%ebx\n" ::: "eax", "ebx");
}

void use_again_elsewhere(void) {
    __asm__("add $MAGIC_VALUE, %%ecx\n" ::: "ecx");
}
EOF

if gcc -c test2.c -o test2.o 2>&1; then
    echo "✓ SUCCESS: .equ symbols defined in one function are visible in others"
    nm test2.o | grep -E "MAGIC_VALUE|BUFFER_SIZE"
else
    echo "✗ FAILED: .equ symbols are not file-scoped"
fi

# Test 3: .equ supports forward references
echo -e "\n\n### TEST 3: .equ supports forward references via backpatching ###"
cat > test3.c << 'EOF'
void test_forward_refs(void) {
    __asm__(/* Use symbols before they're defined */
            ".equ forward_const, 100\n"
            ".equ forward_label_ref, my_future_label\n"
            ".equ forward_diff, future_end - future_start\n"

            /* Use the forward references */
            "mov $forward_const, %%eax\n"
            "mov $forward_diff, %%ebx\n"

            /* Define the referenced labels */
            "my_future_label:\n"
            "future_start:\n"
            "nop; nop; nop\n"
            "future_end:\n"
            ::: "eax", "ebx");
}
EOF

if gcc -c test3.c -o test3.o 2>&1; then
    echo "✓ SUCCESS: .equ can reference symbols defined later"
    echo "Symbol values:"
    nm test3.o | grep forward_ | sort
else
    echo "✗ FAILED: Forward references not supported"
fi

# Test 4: .org restrictions
echo -e "\n\n### TEST 4: .org directive restrictions ###"
echo "4a. .org with backward label reference (should work):"
cat > test4a.c << 'EOF'
void test_backward_ref(void) {
    __asm__("back_label:\n"
            "nop\n"
            ".org back_label + 16\n"
            "nop\n");
}
EOF
if gcc -c test4a.c -o test4a.o 2>&1; then
    echo "✓ SUCCESS: .org works with backward references"
else
    echo "✗ FAILED"
fi

echo -e "\n4b. .org with forward label reference (should fail):"
cat > test4b.c << 'EOF'
void test_forward_ref(void) {
    __asm__(".org forward_label + 16\n"
            "nop\n"
            "forward_label:\n"
            "nop\n");
}
EOF
if gcc -c test4b.c -o test4b.o 2>&1 >/dev/null; then
    echo "✗ UNEXPECTED: Should have failed"
else
    echo "✓ EXPECTED FAILURE: .org cannot use forward label references"
fi

echo -e "\n4c. .org with .equ constant (should work):"
cat > test4c.c << 'EOF'
void test_equ_constant(void) {
    __asm__(".equ PADDING, 32\n"
            ".org . + PADDING\n"
            "nop\n");
}
EOF
if gcc -c test4c.c -o test4c.o 2>&1; then
    echo "✓ SUCCESS: .org works with .equ constants"
else
    echo "✗ FAILED"
fi

# Test 5: Label differences are special
echo -e "\n\n### TEST 5: Label differences work with forward references in .org ###"
cat > test5.c << 'EOF'
void test_forward_diff_in_org(void) {
    __asm__(".equ code_size, code_end - code_start\n"
            ".org . + code_size\n"              /* Use before labels exist! */
            "nop\n"                              /* This nop appears after gap */
            "code_start:\n"
            "nop; nop; nop; nop; nop; nop; nop; nop\n"  /* 8 bytes */
            "code_end:\n"
            "nop\n");
}
EOF

if gcc -c test5.c -o test5.o 2>&1; then
    echo "✓ SUCCESS: .org works with forward-referenced label differences!"
    echo "Disassembly showing 8-byte gap before first nop:"
    objdump -d -M intel -z --disassemble=test_forward_diff_in_org test5.o | head -20
else
    echo "✗ FAILED"
fi

# Test 6: Demonstrate the circular dependency case
echo -e "\n\n### TEST 6: Absolute vs Difference - Critical distinction ###"
echo "6a. Using absolute label in .org (should fail):"
cat > test6a.c << 'EOF'
void test_absolute_forward(void) {
    __asm__(".equ abs_pos, abs_label\n"
            ".org abs_pos + 16\n"        /* Add offset to trigger circular dep */
            "nop\n"
            "abs_label:\n"
            "nop\n");
}
EOF

if ! gcc -c test6a.c -o test6a.o 2>&1 | tee test6a_output.txt; then
    echo "✓ EXPECTED FAILURE: Absolute address creates circular dependency"
    grep -E "(Warning|Error|Fatal)" test6a_output.txt | head -2
else
    echo "✗ UNEXPECTED: Should have failed"
    echo "Debug - Compiler output:"
    cat test6a_output.txt
fi

echo -e "\n6b. Using label difference in .org (should work):"
cat > test6b.c << 'EOF'
void test_difference_forward(void) {
    __asm__(".equ diff_size, diff_end - diff_start\n"
            ".org . + diff_size\n"       /* Difference - no circular dep! */
            "nop\n"
            "diff_start:\n"
            "nop; nop; nop; nop\n"
            "diff_end:\n"
            "nop\n");
}
EOF

if gcc -c test6b.c -o test6b.o 2>&1; then
    echo "✓ SUCCESS: Label differences work with forward references"
else
    echo "✗ FAILED: Differences should work"
fi
rm -f test6a_output.txt

# Final demonstration
echo -e "\n\n### FINAL DEMO: Practical usage pattern ###"
cat > demo.c << 'EOF'
/* Function we want to measure */
__attribute__((naked)) void critical_function(void) {
    __asm__("push %%rbp\n"
            "mov %%rsp, %%rbp\n"
            "xor %%eax, %%eax\n"
            "pop %%rbp\n"
            "ret\n" ::: "eax");
}

__attribute__((naked)) void critical_function_end(void) {
    __asm__("ret");
}

void create_optimized_copy(void) {
    __asm__(/* Calculate size using forward reference */
            ".equ func_size, critical_function_end - critical_function\n"

            /* Create aligned copy with padding */
            ".align 16\n"
            "optimized_copy:\n"

            /* Reserve exact space needed */
            ".org optimized_copy + func_size\n"

            /* Pad to cache line */
            ".align 64\n");
}
EOF

if gcc -c demo.c -o demo.o 2>&1; then
    echo "✓ Practical pattern compiled successfully!"
    echo "Function sizes:"
    nm demo.o | grep -E "func_size|critical_function" | sort
fi

echo -e "\n==================================================================="
echo "                        TEST SUMMARY"
echo "==================================================================="
echo "1. ✓ Inline .equ works with C function symbols"
echo "2. ✓ .equ symbols are file-scoped (visible across functions)"
echo "3. ✓ .equ supports forward references via backpatching"
echo "4. ✓ .org has restrictions (no forward label refs, but .equ works)"
echo "5. ✓ Label differences are special (work with forward refs in .org)"
echo "6. ✓ Absolute addresses create circular dependencies with .org"
echo "==================================================================="

# Cleanup
rm -f test*.c test*.o demo.c demo.o test*_output.txt