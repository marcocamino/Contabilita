!    
! File:   a_costanti_tipi_module.f95
! Author: audacia
!
! Created on 29 aprile 2020, 22.33
!
!questo modulo contiene le definizioni di costanti e tipi comuni a tutta l'applicazione
MODULE a_costanti_tipi_module

    implicit none
    
    integer, parameter :: minScelta = 1
    integer, parameter :: maxScelta = 5
    
    type :: scarico_titoli
        integer :: compagnia
        integer :: agenzia
        integer :: intermediario
        integer :: produttore
        integer :: data_giornale_cassa
        integer :: pagina_giornale_cassa
        integer :: linea_giornale_cassa
        integer :: tipo_operazione
        integer :: classe_polizza
        integer :: numero_polizza
        integer :: numero_appendice
        character(len=20) :: contraente
!        integer :: data_inizio
 !       integer :: data_scadenza
!        character(len=20) :: frequenza_pagamento
!        real :: premio_netto
        
        
    end type scarico_titoli

END MODULE a_costanti_tipi_module
