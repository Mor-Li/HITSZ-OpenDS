import os
import numpy as np
from PIL import Image

def produceImage(file):
    image = Image.open(file)
    resized_image = image.resize((512, 512), Image.ANTIALIAS)
    resized_image.save(file)


if __name__ == '__main__':
    path = r'C:\Users\LeeHITsz\Desktop\picture\CV_result\2'
    datanames = os.listdir(path)
    width, height = np.array(Image.open(path+'/'+datanames[0])).shape[:2]
    for i in datanames:
        produceImage(path+'/'+i)
