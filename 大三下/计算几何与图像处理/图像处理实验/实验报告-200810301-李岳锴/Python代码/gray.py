import cv2


def convert_to_grayscale(image_path, output_path):
    # 读取彩色图像
    image = cv2.imread(image_path)

    # 将彩色图像转换为灰度图像
    gray_image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # 保存灰度图像
    cv2.imwrite(output_path, gray_image)


# 调用函数进行转换和显示
convert_to_grayscale('1.jpg', '1_grey.jpg')
