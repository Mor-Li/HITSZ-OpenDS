figure(10),
Img = imread('melanoma2.jpg');
imagesc(Img,[0,255]);colormap('gray');axis off; axis equal;
BW=roipoly;
uu=double(BW);

% 保存 uu 變量到 mat 文件
save('uu.mat', 'uu');

