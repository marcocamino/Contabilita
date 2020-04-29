!    
! File:   costanti_tipi_module.f95
! Author: audacia
!
! Created on 29 aprile 2020, 22.03
!

MODULE costanti_tipi_module

    implicit none
    
    type :: scarico_titoli_dec1
        integer :: compania
        integer :: agenzia
        integer :: intermediario
        integer :: produttore
        integer :: data_giornale_cassa
        !character(len=32) :: str1
    end type scarico_titoli_dec1
    
END MODULE costanti_tipi_module
