#include <stdio.h>
#include <math.h>
#include <glut.h>

void MidPointLine(int x1, int y1, int x2, int y2, FILE *fp) 
{
    int dx = x2 - x1;
    int dy = y2 - y1;
    int a = -dy;
    int b = dx;
    if (b<0)
    {
        a = -a;
        b = -b;
    }
    double m = fabs(1.0 * dy / dx);
    int sx = (x1 > x2) ? -1 : 1;
    int sy = (y1 > y2) ? -1 : 1;
    int d = (m > 1) ? 2 * sy * b + sx * a : 2 * sx * a + sy * b;
    int x = x1;
    int y = y1;

    while (1) 
    {
        fprintf(fp, "(%d, %d)\n", x, y);
        if (x == x2 && y == y2) 
        {
            break;
        }

        if (d <= 0) 
        {
            if (m > 1)
            {
                if (sy > 0)
                {
                    y += sy;
                    d += 2 * b * sy;
                }
                else
                {
                    x += sx;
                    y += sy;
                    d += 2 * (b * sy + a * sx);
                }
            }
            else
            {
                if (sy > 0)
                {
                    x += sx;
                    y += sy;
                    d += 2 * (b * sy + a * sx);
                }
                else
                {
                    x += sx;
                    d += 2 * a * sx;
                }
            }
        }
        else
        {
            if (m > 1)
            {
                if (sy >= 0)
                {
                    x += sx;
                    y += sy;
                    d += 2 * (b * sy + a * sx);
                }
                else
                {
                    y += sy;
                    d += 2 * b * sy;
                }
            }
            else
            {
                if (sy >= 0)
                {
                    x += sx;
                    d += 2 * a * sx;
                }
                else
                {
                    x += sx;
                    y += sy;
                    d += 2 * (b * sy + a * sx);
                }
            }
        }
    }
}
  
void display() {

    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(0.0, 0.5, 0.5);
    glBegin(GL_POINTS);

    FILE *fp;
    int x, y;

    fp = fopen("midpoint_line_output.txt", "r");  // 打开输出文件
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

void OpenGL_draw(int argc, char** argv, int lx, int ly) 
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(3*lx, 3*ly); // 展示3lx*3ly大小的图形窗口
    glutCreateWindow("MidPoint Line Drawing");
    glClearColor(1.0, 1.0, 1.0, 1.0);
    glMatrixMode(GL_PROJECTION);
    gluOrtho2D(0, 3*lx, 0, 3*ly); //以左下角为坐标轴原点，绘制位于第一象限的像素
    glutDisplayFunc(display);
    glutMainLoop();
    return;
}

int main(int argc, char** argv) 
{ 
    int x1, y1, x2, y2; // 圆心坐标和半径
    FILE *fp; // 文件指针

    printf("请输入两个端点坐标（格式：x1 y1 x2 y2）：");
    scanf("%d %d %d %d", &x1, &y1, &x2, &y2); // 从键盘读取端点坐标

    fp = fopen("midpoint_line_output.txt", "w"); // 打开输出文件
    MidPointLine(x1, y1, x2, y2, fp); // 调用中点画圆函数，并将输出文件指针传递给函数
    fclose(fp); // 关闭输出文件

    OpenGL_draw(argc, argv, abs(x2-x1), abs(y2-y1)); // 调用绘图函数，并将输出文件指针传递给函数 

    return 0; 
} 