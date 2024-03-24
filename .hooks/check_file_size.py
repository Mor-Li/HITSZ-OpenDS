import sys
import os

max_size = 100 * 1024 * 1024  # 100MB

def create_markdown_for_large_file(file_path, download_link_template):
    """Creates a markdown file in the directory of the provided file path with
    instructions to download large files.

    Args:
        file_path (str): The path to a file within the target directory.
        max_size (int): The maximum file size in bytes.
        download_link_template (str): A template string for the download link, which must contain
                                       a placeholder for the filename, e.g., 'http://example.com/{}'.
    """
    # Extract directory path from the file path
    dir_parent_path = os.path.dirname(file_path)
    dir_path = os.path.join(os.path.dirname(file_path), 'internal')
    # List to hold filenames of large files
    large_files = []

    # Iterate over all files in the directory
    for filename in os.listdir(dir_path):
        file_path = os.path.join(dir_path, filename)
        # Skip if not a file
        if not os.path.isfile(file_path):
            continue
        # Check if the file size exceeds the max size
        if os.path.getsize(file_path) > max_size:
            large_files.append(filename)

    # Proceed only if there are large files
    if large_files:
        markdown_file_path = os.path.join(dir_parent_path, "readme.md")
        with open(markdown_file_path, 'w') as md_file:
            md_file.write("# 大文件通知\n\n")
            # Handling single or multiple files in the message
            files_string = "`, `".join(large_files)
            md_file.write(f"文件 `{files_string}` 超出了直接存储的大小限制。\n\n")
            md_file.write(f"请从此链接下载高清版本：[点击这里下载]({download_link})\n")

def move_file_to_internal(file_path, download_link):
    """Moves all files larger than max_size in the directory of the provided
    file path to an 'internal' subdirectory.

    Args:
        file_path (str): The path to a file within the target directory. This is used to determine the directory to scan.
        max_size (int): The maximum file size in bytes. Files larger than this size will be moved.
    """
    dir_path = os.path.dirname(file_path)
    internal_dir = os.path.join(dir_path, "internal")

    # Create the internal directory if it doesn't exist
    if not os.path.exists(internal_dir):
        os.makedirs(internal_dir)

    # Iterate over all files in the directory
    for filename in os.listdir(dir_path):
        current_file_path = os.path.join(dir_path, filename)
        # Skip if not a file or if it's the directory itself
        if not os.path.isfile(current_file_path) or filename == "internal":
            continue
        
        # Move the file if it exceeds the max size
        if os.path.getsize(current_file_path) > max_size:
            new_path = os.path.join(internal_dir, filename)
            os.rename(current_file_path, new_path)
            print(f"Moved: {current_file_path} to {new_path}")

    create_markdown_for_large_file(file_path, download_link)

def check_file_size(file_path, download_link):
    if os.path.isfile(file_path) and os.path.getsize(file_path) > max_size:
        print(f"Error: The file {file_path} is larger than 100MB. Moving to internal directory and creating markdown.")
        move_file_to_internal(file_path, download_link)
        sys.exit(1)

def main(download_link):
    for file_path in sys.argv[1:]:
        check_file_size(file_path, download_link)

if __name__ == "__main__":
    download_link = "https://drive.google.com/drive/folders/1smIdXiMJlrX9ZU0Ag10mBpkT6r2cJwmx?usp=sharing"
    main(download_link)
