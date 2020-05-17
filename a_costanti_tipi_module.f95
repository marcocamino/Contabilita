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
    
    integer, parameter :: num_righe_intestazione_chiusura_titoli = 3
    integer, parameter :: num_righe_intestazione_titoli = 2
    integer, parameter :: num_righe_chiusura_titoli = 1
    
    integer, parameter :: num_righe_intestazione_titoli_dettagliato = 1
    
    CHARACTER(len=100) :: pathNameWorkingDirectory = "C:\Users\audacia\Desktop\fortran\fileInput\"
    
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
        integer :: data_inizio
        integer :: data_scadenza
        character(len=20) :: frequenza_pagamento
        real :: premio_netto
        real :: premio_lordo
        integer :: data_riscossione
        integer :: data_pagamento
        real :: premio_pagato
        real :: commissioni
        real :: imposta_totali
        real :: ssn
        real :: rca_commissioni
        real :: commissioni_cvt
        real :: antiraket_tasse
        character(len=20) :: residenza
        real :: prov_tasse
        character(len=20) :: sigla_targa
        character(len=20) :: nr_targa
        integer :: tipo_veicolo
        character(len=50) :: modello
        character(len=2) :: alimentazione
        character(len=10) :: uso
        integer :: pieno_carico
        integer :: data_immatricolazione
        integer :: eta_conducente
        character(len=10) :: tipo_quietanzamento
    end type scarico_titoli
    
    
    type :: scarico_dettagliato_titoli
        integer :: data_foglio_cassa
        integer :: numero_foglio_cassa
        integer :: riga_foglio_cassa
        integer :: operation_type
        integer :: data_incasso
        character(len=20) :: contraente
        character(len=15) :: cod_fis_par_iva
        integer :: numero_polizza
        integer :: numero_appendice
        integer :: data_decorrenza
        character(len=2) :: frazionamento
        integer :: ramo
        character(len=4) :: pdgar_codgaranzia
        real :: pdgar_netto1rata
        real :: pdgar_tasse1rata
        real :: pdgar_abbuono1rata
        real :: contributo_ssn
        real :: antiraket
        character(len=4) :: provincia_di_residenza
        real :: imposte_provinciali
        character(len=4) :: sigla_targa
        character(len=10) :: numero_targa
    end type scarico_dettagliato_titoli    
    

END MODULE a_costanti_tipi_module
