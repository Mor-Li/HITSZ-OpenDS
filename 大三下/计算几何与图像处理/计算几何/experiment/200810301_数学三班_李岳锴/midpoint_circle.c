#include <stdio.h>
#include <math.h>
#include <glut.h>

void MidPointCircle(int xc, int yc, int r, FILE *fp) 
{ 
    int x = 0, y = r; // 初始化初始点
    int d = 1 - r; // 初始化判别式

    while (y >= x) // 只在1/8圆弧中计算
    { 
        if (x == 0) // 如果是在y轴上，输出4个对称点
        { 
            fprintf(fp, "(%d, %d)\n", xc, yc + y); 
            fprintf(fp,"(%d, %d)\n", xc + y, yc); 
            fprintf(fp, "(%d, %d)\n", xc, yc - y); 
            fprintf(fp, "(%d, %d)\n", xc - y, yc); 
        } 
        else if (x == y) // 如果是在对角线上，输出4个对称点并结束
        {
            fprintf(fp, "(%d, %d)\n", xc + x, yc + y); 
            fprintf(fp, "(%d, %d)\n", xc + x, yc - y); 
            fprintf(fp, "(%d, %d)\n", xc - x, yc - y); 
            fprintf(fp, "(%d, %d)\n", xc - y, yc + y);
            break;
        }
        else // 否则输出8个对称点
        {
            fprintf(fp, "(%d, %d)\n", xc + x, yc + y); 
            fprintf(fp, "(%d, %d)\n", xc + y, yc + x); 
            fprintf(fp, "(%d, %d)\n", xc + y, yc - x); 
            fprintf(fp, "(%d, %d)\n", xc + x, yc - y);
            fprintf(fp, "(%d, %d)\n", xc - x, yc -y); 
            fprintf(fp, "(%d, %d)\n", xc - y, yc - x); 
            fprintf(fp, "(%d, %d)\n", xc - y, yc + x); 
            fprintf(fp, "(%d, %d)\n", xc - x, yc + y);
        }
  
        if (d < 0) // 如果判别式小于0，取下一个像素点
        { 
            d += 2*x + 3; 
        } 
        else // 否则取下一个像素点，并将判别式更新
        { 
            d += 2*(x - y) + 5; 
            y--; // y坐标减1
        } 
        x++; // x坐标加1
    } 
} 
  
void display() {

    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(0.0, 0.5, 0.5);
    glBegin(GL_POINTS);

    FILE *fp;
    int x, y;

    fp = fopen("midpoint_circle_output.txt", "r");  // 打开输出文件
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
    glutCreateWindow("Midpoint Circle Drawing");
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

    fp = fopen("midpoint_circle_output.txt", "w"); // 打开输出文件
    MidPointCircle(xc, yc, r, fp); // 调用中点画圆函数，并将输出文件指针传递给函数
    fclose(fp); // 关闭输出文件

    OpenGL_draw(argc, argv, r); // 调用绘图函数，并将输出文件指针传递给函数 

    return 0; 
} 