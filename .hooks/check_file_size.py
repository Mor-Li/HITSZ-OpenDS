import sys
import os

max_size = 100 * 1024 * 1024  # 100MB

def move_file_to_internal(file_path):
    dir_path = os.path.dirname(file_path)
    internal_dir = os.path.join(dir_path, "internal")
    
    if not os.path.exists(internal_dir):
        os.makedirs(internal_dir)
    
    new_path = os.path.join(internal_dir, os.path.basename(file_path))
    os.rename(file_path, new_path)
    print(f"Moved: {file_path} to {new_path}")

def check_file_size(file_path):
    if os.path.isfile(file_path) and os.path.getsize(file_path) > max_size:
        print(f"Error: The file {file_path} is larger than 100MB. Moving to internal directory.")
        move_file_to_internal(file_path)
        sys.exit(1)

def main():
    for file_path in sys.argv[1:]:
        check_file_size(file_path)

if __name__ == "__main__":
    main()
