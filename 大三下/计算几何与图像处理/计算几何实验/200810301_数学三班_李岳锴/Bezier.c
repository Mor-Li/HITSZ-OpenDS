#include <stdio.h>
#include <stdlib.h>

void Bspline(int m, int n, float* X, float* Y)
{
    if (m==2)
    {    
        for(int i=0; i<n-m; i++)
        {
            printf("第%d段曲线：\n", i+1);
            printf("X的参数方程：%.2ft^2 + %.2ft + %.2f\n", 
                                X[i]/2-X[i+1]+X[i+2]/2, X[i+1]-X[i], X[i]/2+X[i+1]/2);
            printf("Y的参数方程：%.2ft^2 + %.2ft + %.2f\n", 
                                Y[i]/2-Y[i+1]+Y[i+2]/2, Y[i+1]-Y[i], Y[i]/2+Y[i+1]/2);
            printf("起点坐标：(%.2f, %.2f)\n", X[i]/2+X[i+1]/2, Y[i]/2+Y[i+1]/2);
            printf("终点坐标：(%.2f, %.2f)\n", X[i+1]/2+X[i+2]/2, Y[i+1]/2+Y[i+2]/2);
        }
        return;
    }
    else if(m==3)
    {
        for(int i=0; i<n-m; i++)
        {
            printf("第%d段曲线：\n", i+1);
            printf("X的参数方程：%.2ft^3 + %.2ft^2 + %.2ft + %.2f\n", -X[i]/6+X[i+1]/2-X[i+2]/2+X[i+3]/6,
                                X[i]/2-X[i+1]+X[i+2]/2, X[i+2]/2-X[i]/2, X[i]/6+2*X[i+1]/3+X[i+2]/6);
            printf("Y的参数方程：%.2ft^3 + %.2ft^2 + %.2ft + %.2f\n", -Y[i]/6+Y[i+1]/2-Y[i+2]/2+Y[i+3]/6,
                                Y[i]/2-Y[i+1]+Y[i+2]/2, Y[i+2]/2-Y[i]/2, Y[i]/6+2*Y[i+1]/3+Y[i+2]/6);
            printf("起点坐标：(%.2f, %.2f)\n", X[i]/6+2*X[i+1]/3+X[i+2]/6, Y[i]/6+2*Y[i+1]/3+Y[i+2]/6);
            printf("终点坐标：(%.2f, %.2f)\n", X[i+1]/6+2*X[i+2]/3+X[i+3]/6, Y[i+1]/6+2*Y[i+2]/3+Y[i+3]/6);
        }
        return;
    }
}

int main() {

    int m; // 设置B样条的次数
    printf("请输入B样条曲线的次数：");
    scanf("%d", &m);

    int n; // 设置控制点的个数
    printf("请输入控制点的个数：");
    scanf("%d", &n);

    float *x_coords = (float *)malloc(n * sizeof(float));
    float *y_coords = (float *)malloc(n * sizeof(float));

    printf("Enter %d points (x,y):\n", n);
    for (int i = 0; i < n; i++) 
    {
        scanf("%f %f", &x_coords[i], &y_coords[i]);
        // 处理读取到的坐标点
        printf("Read point: (%.2f, %.2f)\n", x_coords[i], y_coords[i]);
    }

    Bspline(m, n, x_coords, y_coords);

    free(x_coords);
    free(y_coords);

    return 0;
}