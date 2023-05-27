clc
clear all

%------- Open the text file for x ------%
fileID_x = fopen('output_x_pdp_hiperbolik_tugas10.txt', 'r');

% Skip the first line
headerLines = 1;
for i = 1:headerLines
    fgetl(fileID_x);
end

% Read the remaining data into a variable
x = textscan(fileID_x, '%f');
x = x{1};  % Extract the data from the cell array

% Close the file
fclose(fileID_x);

%------- Open the text file for t ------%
fileID_t = fopen('output_t_pdp_hiperbolik_tugas10.txt', 'r');

% Skip the first line
headerLines = 1;
for i = 1:headerLines
    fgetl(fileID_t);
end

% Read the remaining data into a variable
t = textscan(fileID_t, '%f');
t = t{1};  % Extract the data from the cell array

% Close the file
fclose(fileID_t);

n = size(x);
m = size(t);

% Open the text file
fileID_y = fopen('output_y_pdp_hiperbolik_tugas10.txt', 'r');

% Skip the first line
headerLines = 1;
for i = 1:headerLines
    fgetl(fileID_y);
end

% Read the remaining data into a variable
data = textscan(fileID_y, '%f', 'Delimiter', '\t');
fclose(fileID_y);

% Extract the matrix dimensions
numRows = n(1,1)+1;
numCols = m(1,1);

% Reshape the data into a matrix
y = reshape(data{1}, numCols, numRows - 1).';

% Transpose the matrix to match the desired size
y = y.';  % Size: 101x101

% Create a meshgrid of x and y coordinates
[X, T] = meshgrid(x, t);

% Plot the 3D surface
surf(X, T, y);
xlabel('x');
ylabel('t');
zlabel('y');
title('Grafik Simpangan dawai sebagai fungsi posisi(x) dan waktu(t)');

% Customize the plot appearance (optional)
colormap('jet');  % Set the colormap
colorbar;         % Add a colorbar
axis tight;       % Set tight axis limits
grid on;          % Show grid lines

% Menyimpan gambar plot grafik
saveas(gcf, 'plot_simpangan_gelombang', 'png');
