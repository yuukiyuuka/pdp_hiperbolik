program pdp_hiperbolik
implicit none
integer, parameter :: dpr = kind(1.0D0)
real(dpr) :: v_gelombang, tegangan, massa, panjang_dawai
real(dpr) :: ht, hx, xmin, xmax, tmin, tmax, hu
real(dpr), allocatable :: x(:), t(:), b(:), y_x_0(:), y(:,:)
integer :: i, j, kx, kt, ky
real(dpr) :: y_0_t, y_100_t

!membaca data karakteristik dawai
  open(unit=10, file="data_karakteristik_dawai.txt", status='old', action='read')

  ! Skip the first line
  read(10, *)

  ! Read the data from the file
  read(10, *) tegangan, massa, panjang_dawai

  ! Close the input file
  close(unit=10)

!menghitung kecepatan gelombang pada dawai

v_gelombang = kecepatan_gelombang_dawai(tegangan,massa,panjang_dawai)

write(*,*) "Kecepatan gelombang pada dawai =", v_gelombang, "m/s"

!membaca nilai step posisi x dan waktu t
write(*,*) "Masukkan step waktu ht dan step posisi hx yang diinginkan"
write(*,*) "ht ="
read(*,*) ht
write(*,*) "hx ="
read(*,*) hx

!mengenerate data nilai x untuk output
xmax = 100.0 !dalam cm
xmin = 0.0   !dalam cm
kx = 1 + floor((xmax-xmin)/hx) !jumlah titik data output x

allocate(x(kx))

do i = 1,kx
   x(i) = xmin + (i-1)*hx
end do

!menyimpan data nilai x ke file eksternal
open(unit=20, file="output_x_pdp_hiperbolik_tugas10.txt", status="replace", action="write")
write(20,*) "jumlah data output x =", kx
do i = 1,kx
   write(20,*) x(i)
end do
close(20)

!menentukan sampai kapan (tmax) hasil ingin dihitung
write(*,*) "Masukkan nilai waktu maksimum pengambilan data" 
write(*,*) "dalam detik, tmax ="
read(*,*) tmax

!mengenerate data nilai t untuk output
tmin = 0.0   !dalam s
kt = 1 + floor((tmax-tmin)/ht) !jumlah titik data output t

allocate(t(kt))

do i = 1,kt
   t(i) = tmin + (i-1)*ht
end do

!menyimpan data nilai t ke file eksternal
open(unit=30, file="output_t_pdp_hiperbolik_tugas10.txt", status="replace", action="write")
write(30,*) "jumlah data output t =", kt
do i = 1,kt
   write(30,*) t(i)
end do
close(30)

!!!!!----- menghitung solusi numerik -----!!!!!!

allocate(y_x_0(kx))

!nilai batas
do i = 1,kx
   y_x_0(i) = simpangan_awal(x(i))      !simpangan awal
end do
y_0_t = 0.0                             !ujung kiri terikat
y_100_t = 0.0                           !ujung kanan terikat

!jumlah data y
ky = kx*kt

!inisiasi simpangan y
allocate(y(kx,kt))
do i = 1,kx
   do j = 1,kt
      y(i,j) = 0.0
   end do
end do

!menyimpan syarat batas ke array y(i,j)
do i = 1,kx-1
   y(i,1) = y_x_0(i)
end do

do j = 1,kt
   y(1,j) = y_0_t
   y(kx,j) = y_100_t
end do

!gradien simpangan awal dy/dx_0 = b
allocate(b(kx))
do i = 1,kx
   b(i) = gradien_simpangan_awal(x(i))
end do
!menghitung y(i,j) setiap waktu dengan metode numerik pdp parabolik

!untuk j = 2
do i = 2,kx-1
   y(i,2) = b(i)*ht + y(i,1) + (v_gelombang*ht/hx)**2 * (y(i+1,1) - 2*y(i,1) + y(i-1,1)) / 2.0
end do

do j = 3,kt
   do i = 2,kx-1
      y(i,j) = 2*y(i,j-1) - y(i,j-2) + (v_gelombang*ht/hx)**2 * (y(i+1,j-1) - 2*y(i,j-1) + y(i-1,j-1)) 
   end do
end do

!menyimpan data nilai y ke file eksternal
open(unit=40, file="output_y_pdp_hiperbolik_tugas10.txt", status="replace", action="write")
write(40,*) "jumlah data output y =", ky
do i = 1,kx
   write(40,*) y(i,:)
end do
close(40)

deallocate(x,t,y_x_0,y,b)

stop

contains

function kecepatan_gelombang_dawai(T,m,L) result(v)
implicit none
real(dpr), intent(in) :: T,m,L
real(dpr) :: v

v = sqrt(1000*T*L/m)

end function kecepatan_gelombang_dawai

function simpangan_awal(x) result(y0)
implicit none
real(dpr), intent(in) :: x 
real(dpr) :: y0

if (x <= 20) then
   y0 = 0.05*x
else
   y0 = 1.25 - 0.0125*x
end if

end function simpangan_awal

function gradien_simpangan_awal(x) result(dy_dx_0)
implicit none
real(dpr), intent(in) :: x 
real(dpr) :: dy_dx_0

if (x <= 20) then
   dy_dx_0 = 0.05
else
   dy_dx_0 = - 0.0125
end if

end function gradien_simpangan_awal

end program pdp_hiperbolik