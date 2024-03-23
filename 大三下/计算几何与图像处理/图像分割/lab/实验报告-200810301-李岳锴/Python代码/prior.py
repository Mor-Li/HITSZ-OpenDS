import cv2
import numpy as np
import matplotlib.pyplot as plt


# 定义用于绘制先验分割框的全局变量
refPt = []
drawing = False

# 定义模型参数
file = '4'
iterations = 100
alpha = 300
beta0 = 300
epsilon = 1
lambda0 = 750
lambda1 = 1
lambda2 = 1
sigma = 16

# 定义核函数
kernel = cv2.getGaussianKernel(int(round(4 * sigma)) + 1, sigma)
kernel = np.dot(kernel, kernel.T)

"""定义鼠标绘制先验图像分割框函数"""
def click_and_crop(event, x, y, flags, param):
    global refPt, drawing, image_prior

    if event == cv2.EVENT_LBUTTONDOWN:
        refPt = [(x, y)]
        drawing = True
    elif event == cv2.EVENT_LBUTTONUP:
        refPt.append((x, y))
        drawing = False

        # 绘制矩形框
        cv2.rectangle(image_prior, refPt[0], refPt[1], (0, 0, 255), 2)
        cv2.imshow("image", image_prior)


"""定义近似Heaviside函数"""
def heaviside_epsilon(z):
    return 0.5 * (1 + (2 / np.pi) * np.arctan(z / epsilon))


"""定义结合先验约束项的图像分割模型"""
def prior_RSF(phi, phi_prior, img, sx, sy, hx, hy):

    Hea = heaviside_epsilon(phi)

    f1 = cv2.filter2D(Hea * img, -1, kernel) / cv2.filter2D(Hea, -1, kernel)
    f2 = cv2.filter2D((1 - Hea) * img, -1, kernel) / cv2.filter2D(1 - Hea, -1, kernel)

    T1 = (lambda1 - lambda2) * img**2
    T2 = 2 * lambda2 * img * cv2.filter2D(f2, -1, kernel) - 2 * lambda1 * img * cv2.filter2D(f1, -1, kernel)
    T3 = lambda1 * cv2.filter2D(f1**2, -1, kernel) - lambda2 * cv2.filter2D(f2**2, -1, kernel)

    T = T1 + T2 + T3

    v1 = np.pad(np.diff(sx, axis=1), ((0, 0), (1, 0)), mode='constant') + np.pad(np.diff(sy, axis=0),
                                                                                 ((1, 0), (0, 0)), mode='constant')
    v2 = np.pad(np.diff(hx, axis=1), ((0, 0), (1, 0)), mode='constant') + np.pad(np.diff(hy, axis=0),
                                                                                 ((1, 0), (0, 0)), mode='constant')
    v3 = np.pad(phi[:-1, :], ((1, 0), (0, 0)), mode='constant') + np.pad(phi[1:, :], ((0, 1), (0, 0)),
                                                                         mode='constant') + \
         np.pad(phi[:, :-1], ((0, 0), (1, 0)), mode='constant') + np.pad(phi[:, 1:], ((0, 0), (0, 1)),
                                                                         mode='constant')
    xi = v1 - v2
    beta = (lambda0 * v3 - T + lambda0 * xi) + alpha * phi_prior
    phi = beta / (alpha + 4 * lambda0)

    dx, dy = np.gradient(img)
    g = 1 / (1 + beta0 * (dx**2 + dy**2))

    phix, phiy = np.gradient(phi)
    s1 = hx + phix
    s2 = hy + phiy
    sx = s1 / np.abs(s1) * np.maximum(np.abs(s1) - g / lambda0, 0)
    sy = s2 / np.abs(s2) * np.maximum(np.abs(s2) - g / lambda0, 0)
    hx = hx + phix - sx
    hy = hy + phiy - sy

    return phi, sx, sy, hx, hy


"""定义绘制图像的函数"""
def drawContour(phi, image, iteration=None):
    plt.figure(figsize=(8, 8))
    plt.imshow(image)
    plt.xticks([])
    plt.yticks([])
    plt.contour(phi, [0], colors='r', linewidths=2)
    plt.draw()
    if iteration is not None:
        plt.title("{:d} iterations".format(iteration))
        plt.savefig(fname='prior_result/'+file+'/'+"{:04d}".format(iteration)+"."+"jpeg",
                    bbox_inches='tight', pad_inches=0.1)
    else:
        plt.show()
    plt.close()
    return


# 读取图像
input_image = cv2.imread(file+'.jpg', 1)
img_float = np.array(cv2.cvtColor(input_image, cv2.COLOR_BGR2GRAY), dtype=np.float64)

# 使用鼠标绘制先验图像分割框
# 放大图像尺寸
scale_percent = 2  # 放大的比例
width = int(input_image.shape[1] * scale_percent)
height = int(input_image.shape[0] * scale_percent)
image_prior = cv2.resize(input_image, (width, height))

cv2.namedWindow("image_prior")
cv2.setMouseCallback("image_prior", click_and_crop)

while True:
    cv2.imshow("image_prior", image_prior)
    key = cv2.waitKey(1) & 0xFF

    # 退出程序
    if key == 27:
        break

cv2.destroyAllWindows()

# 初始化
sx, sy = np.ones(img_float.shape), np.ones(img_float.shape)
hx, hy = np.ones(img_float.shape), np.ones(img_float.shape)
LSF = -np.ones((img_float.shape[0], img_float.shape[1]), img_float.dtype)
# LSF[11:45, 14:45] = 1
refPt = (np.array(refPt)/scale_percent).astype(int)
LSF[refPt[0][1]:refPt[1][1], refPt[0][0]:refPt[1][0]] = 1

# 绘制初始轮廓
input_image = cv2.cvtColor(input_image, cv2.COLOR_BGR2RGB)
drawContour(LSF, input_image, 0)

# 迭代过程
LSF_prior = LSF.copy()
for i in range(1, iterations+1):
    LSF, sx, sy, hx, hy = prior_RSF(LSF, LSF_prior, img_float, sx, sy, hx, hy)
    drawContour(LSF, input_image, i)

# os.system(r"cd C:\Users\LeeHITsz\Desktop\picture\prior_result\4")
# os.system(r"ffmpeg -i 0%03d.jpeg -y -qscale 0.1 -filter setpts=1.5*PTS movie.mp4")
