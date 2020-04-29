!    
! File:   a_costanti_tipi_module.f95
! Author: audacia
!
! Created on 29 aprile 2020, 22.33
!

MODULE a_costanti_tipi_module

    implicit none
    
    integer, parameter :: minScelta = 1
    integer, parameter :: maxScelta = 5
    
    type :: scarico_titoli
        integer :: compania
        integer :: agenzia
        integer :: intermediario
        integer :: produttore
        integer :: data_giornale_cassa
        !character(len=32) :: str1
    end type scarico_titoli

END MODULE a_costanti_tipi_module