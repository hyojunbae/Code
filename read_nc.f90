!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! For compile you should use this option in polar1 sever
!
! gfortran read_nc.f90 -I. -I/usr/local/netcdf/413_gcc485/include -L/usr/local/netcdf/413_gcc485/lib -lnetcdf -lnetcdff
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PROGRAM nc_read
!!! hello world !!!!

!!!! now test !!!
 use netcdf ! use netCDF libraries in fortran
 IMPLICIT NONE

 CHARACTER(LEN=100) :: infile, ofile
 CHARACTER(LEN=10) :: var_name
 INTEGER(KIND=8) :: reclen
 CHARACTER(LEN=*), PARAMETER :: infile_path = "/data2/Reanalysis/JRA55/daily/anl_p125/tmp/"
 CHARACTER(LEN=*), PARAMETER :: infile_name = "anl_p125_tmp.day."
 CHARACTER(LEN=*), PARAMETER :: file_format = ".nc"
 CHARACTER(LEN=*), PARAMETER :: ofile_path = "/data1/hjbae/1.data/1.JRA/"
 CHARACTER(LEN=*), PARAMETER :: ofile_name = "JRA_p125_tmp.bin"

 INTEGER :: str_year, str_mon, end_year, end_mon, ntimes
 INTEGER :: ncid, xtype, ndims, var_id
 INTEGER :: ntime, nlev, nlat, nlon, count, time_count, nrec
 INTEGER, PARAMETER :: ndim = 5 ! (var=1, time=2, lev=3, lat=4, lon=5)
 INTEGER, PARAMETER :: nlevs = 37, nlats = 145, nlons = 288
 INTEGER, DIMENSION(ndim) :: dimids 

 REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: raw
 REAL, DIMENSION(nlons,nlats,nlevs,15000) :: var
 INTEGER, ALLOCATABLE :: time(:)
 INTEGER, DIMENSION(15000) :: var_time
 REAL :: lev(nlevs), lat(nlats), lon(nlons)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! time setting
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 str_year = 1981 
 str_mon  = 1
 end_year = 1984 
 end_mon  = 12 

 count = 0
 time_count = 0
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! read nc file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 do

 write(infile,'(a,a,i4,i2.2,a,a)')infile_path,infile_name,str_year,str_mon,file_format

 CALL check(nf90_open(infile, nf90_nowrite, ncid)) ! open existing netCDF dataset
 CALL check(nf90_inquire_dimension(ncid, 1, var_name, ntimes)) ! get dimension names, lengths
 
 ALLOCATE(raw(nlons, nlats, nlevs, ntimes))
 ALLOCATE(time(ntimes))

 CALL check(nf90_inquire_variable(ncid, 1, var_name, xtype, ndims, dimids)) ! get dimension names, lengths 
 CALL check(nf90_inq_varid(ncid, var_name, var_id)) ! get var_id
 CALL check(nf90_get_var(ncid, var_id, raw)) ! get values of variables

 CALL check(nf90_inquire_variable(ncid, 2, var_name, xtype, ndims, dimids))
 CALL check(nf90_inq_varid(ncid, var_name, var_id))
 CALL check(nf90_get_var(ncid, var_id, time))

 CALL check(nf90_inquire_variable(ncid, 3, var_name, xtype, ndims, dimids))
 CALL check(nf90_inq_varid(ncid, var_name, var_id))
 CALL check(nf90_get_var(ncid, var_id, lev))

 CALL check(nf90_inquire_variable(ncid, 4, var_name, xtype, ndims, dimids))
 CALL check(nf90_inq_varid(ncid, var_name, var_id))
 CALL check(nf90_get_var(ncid, var_id, lat))

 CALL check(nf90_inquire_variable(ncid, 5, var_name, xtype, ndims, dimids))
 CALL check(nf90_inq_varid(ncid, var_name, var_id))
 CALL check(nf90_get_var(ncid, var_id, lon))

 do ntime = 1, ntimes

    count = count + 1
    time_count = time_count + 1
    var_time(count) = str_year*1000 + time_count
    print*, "check time", var_time(count) 
    do nlev = 1, nlevs
       do nlat = 1, nlats
          do nlon = 1, nlons
             var(nlon,nlat,nlev,count) = raw(nlon,nlat,nlev,ntime)
          end do
       end do
    end do

 end do

 DEALLOCATE(raw)
 DEALLOCATE(time)

 CALL check(nf90_close(ncid))  ! close netCDF dataset

 str_mon = str_mon + 1
   if(str_year .eq. end_year .and. str_mon .eq. end_mon + 1) exit

      if(str_mon .eq. 13) then
        str_mon = 1
        str_year = str_year + 1
        time_count = 0
      end if

 end do

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! make output file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 inquire(iolength=reclen) var(:,:,:,1)

 write(ofile,'(a,a)')ofile_path,ofile_name
 open (unit=11, file=ofile, form="unformatted",status="unknown",access="direct",recl=reclen)
 do nrec = 1, count
 write (unit=11,rec=nrec)(((var(nlon,nlat,nlev,nrec),nlon=1,nlons),nlat=1,nlats),nlev=1,nlevs)
 end do

 close(unit=11)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! The subroutine readgrid utilizes the subroutine check, which will return a
! netCDF error and stop execution of the code if netCDF encounters an error. 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 CONTAINS
 SUBROUTINE check(istatus)
 INTEGER, INTENT(IN) :: istatus
    
 if(istatus /= nf90_noerr) then 
   write(*,*) trim(nf90_strerror(istatus))
   stop "Stopped"
 end if

 END SUBROUTINE check 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

END PROGRAM
