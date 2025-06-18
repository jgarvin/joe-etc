{
  description = "Development environment with unoptimized glibc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build glibc without optimizations
        glibcDebug = pkgs.glibc.overrideAttrs (oldAttrs: {
          pname = "glibc-debug";

          # Use env attribute for environment variables
          env = (oldAttrs.env or {}) // {
            NIX_CFLAGS_COMPILE = toString [
              (oldAttrs.env.NIX_CFLAGS_COMPILE or "")
              "-Og"
              "-ggdb"
              "-fno-omit-frame-pointer"
            ];
          };

          # Keep debug symbols
          dontStrip = true;
          separateDebugInfo = false;

          # Disable hardening features
          hardeningDisable = [ "fortify" "stackprotector" ];

          configureFlags = oldAttrs.configureFlags ++ [
            "--disable-werror"
          ];
        });

        # Create a compile script that uses the debug glibc
        compileWithDebugGlibc = pkgs.writeShellScriptBin "compile-debug" ''
          if [ $# -eq 0 ]; then
            echo "Usage: compile-debug <source-file> [gcc-options...]"
            exit 1
          fi

          SOURCE="$1"
          shift

          OUTPUT="''${SOURCE%.c}"

          echo "Compiling with debug glibc..."
          ${pkgs.gcc}/bin/gcc \
            -o "$OUTPUT" \
            "$SOURCE" \
            -Wl,--dynamic-linker=${glibcDebug}/lib/ld-linux-x86-64.so.2 \
            -Wl,-rpath=${glibcDebug}/lib \
            "$@"

          echo "Compiled to: $OUTPUT"
          echo "Linked against: ${glibcDebug}/lib"
        '';

      in
        {
          packages.glibcDebug = glibcDebug;

          devShells.default = pkgs.mkShell {
            # without this nix just turns optimization back on
            hardeningDisable = [ "all" ];

            buildInputs = with pkgs; [
              gcc
              gdb
              valgrind
              strace
              compileWithDebugGlibc
            ];

            shellHook = ''
            echo "Development shell with debug glibc compilation support"
            echo ""
            echo "To compile with unoptimized glibc:"
            echo "  compile-debug test.c"
            echo ""
            echo "Or manually:"
            echo "  gcc -o test test.c -Wl,--dynamic-linker=${glibcDebug}/lib/ld-linux-x86-64.so.2 -Wl,-rpath=${glibcDebug}/lib"
            echo ""
            echo "Debug glibc location: ${glibcDebug}/lib"
          '';
          };
        });
}