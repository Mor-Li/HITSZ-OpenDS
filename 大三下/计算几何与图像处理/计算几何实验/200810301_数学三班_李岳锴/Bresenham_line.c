#include <stdio.h>
#include <math.h>
#include <glut.h>

void BresenhamLine(int x1, int y1, int x2, int y2, FILE *fp) 
{
    int dx = x2 - x1;
    int dy = y2 - y1;
    double m = fabs(1.0 * dy / dx);
    int sx = (x1 < x2) ? 1 : -1;
    int sy = (y1 < y2) ? 1 : -1;
    int e = (m > 1) ? 2 * abs(dx) - abs(dy) : 2 * abs(dy) - abs(dx);
    int x = x1;
    int y = y1;

    while (1) 
    {
        fprintf(fp, "(%d, %d)\n", x, y);
        if (x == x2 && y == y2) {
            break;
        }

        if (e < 0) 
        {
            if (m > 1)
            {
                y += sy;
                e += 2 * abs(dx);
            }
            else
            {
                x += sx;
                e += 2 * abs(dy);
            }
        }
        else
        {
            x += sx;
            y += sy;
            if (m > 1)
            {
                e += 2 * abs(dx) - 2 * abs(dy);
            }
            else
            {
                e += 2 * abs(dy) - 2 * abs(dx);
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

    fp = fopen("Bresenham_line_output.txt", "r");  // 打开输出文件
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
    glutCreateWindow("Bresenham Line Drawing");
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

    fp = fopen("Bresenham_line_output.txt", "w"); // 打开输出文件
    BresenhamLine(x1, y1, x2, y2, fp); // 调用中点画圆函数，并将输出文件指针传递给函数
    fclose(fp); // 关闭输出文件

    OpenGL_draw(argc, argv, abs(x2-x1), abs(y2-y1)); // 调用绘图函数，并将输出文件指针传递给函数 

    return 0; 
} 