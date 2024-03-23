import os
import cv2
import numpy as np
import matplotlib.pyplot as plt


# 定义模型参数
file = '1'
mu = 1
nu = 0.001 * 255 * 255
iterations = 100
epsilon = 1
step = 0.2
lambda1 = 1
lambda2 = 1
sigma = 16

# 定义高斯核函数
kernel = cv2.getGaussianKernel(int(round(4 * sigma)) + 1, sigma)
kernel = np.dot(kernel, kernel.T)

"""定义近似Heaviside函数"""
def heaviside_epsilon(z):
    return 0.5 * (1 + (2 / np.pi) * np.arctan(z / epsilon))


"""定义近似Dirac函数"""
def dirac_epsilon(z):
    return (epsilon / np.pi) / (epsilon * epsilon + z * z)


"""定义RSF模型"""
def RSF(phi, img):

    Hea = heaviside_epsilon(phi)
    Drc = dirac_epsilon(phi)

    Iy, Ix = np.gradient(phi)
    s = np.sqrt(Ix * Ix + Iy * Iy)
    Nx = Ix / (s + 0.000001)
    Ny = Iy / (s + 0.000001)
    Mxx, Nxx = np.gradient(Nx)
    Nyy, Myy = np.gradient(Ny)
    cur = Nxx + Nyy

    Length = nu * Drc * cur

    Lap = cv2.Laplacian(phi, -1)
    Penalty = mu * (Lap - cur)

    f1 = cv2.filter2D(Hea * img, -1, kernel) / cv2.filter2D(Hea, -1, kernel)
    f2 = cv2.filter2D((1 - Hea) * img, -1, kernel) / cv2.filter2D(1 - Hea, -1, kernel)

    R1 = (lambda1 - lambda2) * img ** 2
    R2 = 2 * lambda2 * img * cv2.filter2D(f2, -1, kernel) - 2 * lambda1 * img * cv2.filter2D(f1, -1, kernel)
    R3 = lambda1 * cv2.filter2D(f1 ** 2, -1, kernel) - lambda2 * cv2.filter2D(f2 ** 2, -1, kernel)
    RSFterm = -Drc * (R1 + R2 + R3)

    return phi + step * (RSFterm + Length + Penalty)


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
        plt.savefig(fname='RSF_result/'+file+'/'+"{:04d}".format(iteration)+"."+"jpeg",
                    bbox_inches='tight', pad_inches=0.1)
    else:
        plt.show()
    plt.close()
    return


# 读取图像
input_image = cv2.imread(file+'.bmp', 1)
img_float = np.array(cv2.cvtColor(input_image, cv2.COLOR_BGR2GRAY), dtype=np.float64)

# 初始化水平集函数
LSF = -np.ones((img_float.shape[0], img_float.shape[1]), img_float.dtype)
LSF[11:45, 14:45] = 1

# 绘制初始轮廓
input_image = cv2.cvtColor(input_image, cv2.COLOR_BGR2RGB)
drawContour(LSF, input_image, 0)

# 迭代过程
for i in range(1, iterations+1):
    LSF = RSF(LSF, img_float)
    drawContour(LSF, input_image, i)

# os.system(r"cd C:\Users\LeeHITsz\Desktop\picture\RSF_result\1")
# os.system(r"ffmpeg -i 0%03d.jpeg -y -qscale 0.1 -filter setpts=1.5*PTS movie.mp4")
