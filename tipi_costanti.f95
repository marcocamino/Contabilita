!     
! File:   tipi_costanti.f95
! Author: audacia
!
! Created on 27 aprile 2020, 22.30
!

MODULE tipi_costanti

    implicit none
    
    type :: scarico_titoli
        integer :: compania
        integer :: agenzia
        integer :: intermediario
        integer :: produttore
        integer :: data_giornale_cassa
        !character(len=32) :: str1
    end type scarico_titoli
    
    contains
    
END MODULE tipi_costanti
