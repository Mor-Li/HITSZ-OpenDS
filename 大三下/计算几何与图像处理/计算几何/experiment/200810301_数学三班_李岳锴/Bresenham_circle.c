#include <stdio.h>
#include <math.h>
#include <glut.h>

void BresenhamCircle(int xc, int yc, int r, FILE *fp) 
{ 
    int x = 0, y = r; // 初始化初始点
    int delta = 2*(1 - r); // 初始化判别式

    while (y > 0) // 只在1/4圆弧中计算
    { 
        if (x == 0) // 如果是在y轴上，输出4个坐标轴上的点
        { 
            fprintf(fp, "(%d, %d)\n", xc, yc + y); 
            fprintf(fp,"(%d, %d)\n", xc + y, yc); 
            fprintf(fp, "(%d, %d)\n", xc, yc - y); 
            fprintf(fp, "(%d, %d)\n", xc - y, yc); 
        } 
        else // 否则输出4个对称点
        {
            fprintf(fp, "(%d, %d)\n", xc + x, yc + y); 
            fprintf(fp, "(%d, %d)\n", xc + x, yc - y);
            fprintf(fp, "(%d, %d)\n", xc - x, yc -y); 
            fprintf(fp, "(%d, %d)\n", xc - x, yc + y);
        }
  
        if (delta < 0) // 判别式小于0时，考虑delta_HD的符号
        {
            int delta_HD = 2 * (delta + y) - 1;
            if (delta_HD >= 0)
            {
                delta += 2 * (++x - (--y) + 1);
            }
            else
            {
                delta += 2 * (++x) + 1;
            }
        } 
        else if (delta > 0) // 判别式大于0时，考虑delta_DV的符号
        { 
            int delta_DV = 2 * (delta - x) - 1;
            if (delta_DV < 0)
            {
                delta += 2 * (++x - (--y) + 1);
            }
            else
            {
                delta += (-2) * (--y) + 1;
            }
        }
        else
        {
            delta += 2 * (++x - (--y) + 1);
        } 
    } 
} 
  
void display() {

    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(0.0, 0.5, 0.5);
    glBegin(GL_POINTS);

    FILE *fp;
    int x, y;

    fp = fopen("Bresenham_circle_output.txt", "r");  // 打开输出文件
    if (fp == NULL) 
    {
        printf("Error: Failed to open output file.\n");
        return;
    }
    while (fscanf(fp, "(%d, %d)\n", &x, &y) == 2) // 依次读取每个坐标点
    {  
        glVertex2i(x, y);
    }
    fclose(fp);  // 关闭输出文件

    glEnd();
    glFlush();
}

void OpenGL_draw(int argc, char** argv, int r) 
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(3*r, 3*r); // 展示3r*3r大小的图形窗口
    glutCreateWindow("Bresenham Circle Drawing");
    glClearColor(1.0, 1.0, 1.0, 1.0);
    glMatrixMode(GL_PROJECTION);
    gluOrtho2D(0, 3*r, 0, 3*r); //以左下角为坐标轴原点，绘制位于第一象限的像素
    glutDisplayFunc(display);
    glutMainLoop();
    return;
}

int main(int argc, char** argv) 
{ 
    int xc, yc, r; // 圆心坐标和半径
    FILE *fp; // 文件指针

    printf("请输入圆心坐标和半径（格式：x y r）：");
    scanf("%d %d %d", &xc, &yc, &r); // 从键盘读取圆心坐标和半径

    fp = fopen("Bresenham_circle_output.txt", "w"); // 打开输出文件
    BresenhamCircle(xc, yc, r, fp); // 调用中点画圆函数，并将输出文件指针传递给函数
    fclose(fp); // 关闭输出文件

    OpenGL_draw(argc, argv, r); // 调用绘图函数，并将输出文件指针传递给函数 

    return 0; 
} 