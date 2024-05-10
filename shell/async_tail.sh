#!/usr/bin/env bash

# Initialize variables
log_file=""

# Process script arguments
while [[ "$#" -gt 0 ]]; do
    case "$1" in
        --log)
            log_file="$2"
            shift 2
            ;;
        --) # End of script arguments
            shift
            break
            ;;
        *) # Unknown option
            echo "Unknown option: $1. Did you forget to use -- to begin the subcmd arguments?" >&2
            exit 1
            ;;
    esac
done

if [ -z "$log_file" ]; then
    echo "async_tail requires --log argument." >&2
    exit 1
fi

"$@" &> "$log_file" &
cmd_pid=$!

tail -f "$log_file" &
tail_pid=$!

# By waiting until cmd is finished, we make sure that tail has had
# time to open the output file.
set +x
wait $cmd_pid
exit_code=$?

# Determine the file descriptor for the output file
fd_number=$(ls -l /proc/$tail_pid/fd | grep "$log_file" | awk '{print $9}')

# Path to the fdinfo for the file descriptor being used by tail
fdinfo_path="/proc/$tail_pid/fdinfo/$fd_number"

# now wait until tail's output descriptor has reached
while true; do
    # Get the current size of the file
    current_size=$(stat --format="%s" "$log_file")

    # Get the current offset of the file descriptor from /proc
    current_offset=$(grep pos "$fdinfo_path" | awk '{print $2}')

    # Check if the current offset has reached or exceeded the file size
    if [[ "$current_offset" -ge "$current_size" ]]; then
        echo "Reached the end of the file."
        break
    fi

    # Optional: sleep to prevent tight looping
    sleep 0.1
done

kill $tail_pid || true
wait $tail_pid 2> /dev/null

exit $exit_code
