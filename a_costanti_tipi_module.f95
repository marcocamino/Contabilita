!    
! File:   a_costanti_tipi_module.f95
! Author: audacia
!
! Created on 29 aprile 2020, 22.33
!
! questo modulo contiene le definizioni di costanti e tipi comuni a tutta l'applicazione 
! versione 1.1 del 20200606

MODULE a_costanti_tipi_module

    implicit none
    
    integer, parameter :: minScelta = 1
    integer, parameter :: maxScelta = 5
    
    integer, parameter :: maxPrecisione = 16
    
    integer, parameter :: unitNumber = 18
    
    integer, parameter :: lunghezzaPathName = 100
    integer, parameter :: lunghezzaSingoloCampo = 100
    integer, parameter :: lunghezzaSingoloCampoShort = 10
    integer, parameter :: lunghezzaSingoloCampoMiddle = 20
    integer, parameter :: lunghezzaSingolaRigaCSV = 300
    
    integer, parameter :: num_righe_intestazione_chiusura_titoli = 3
    integer, parameter :: num_righe_intestazione_titoli = 2
    integer, parameter :: num_righe_chiusura_titoli = 1
    
    integer, parameter :: num_righe_intestazione_titoli_dettagliato = 1
    
    CHARACTER(len=100) :: pathNameWorkingDirectory = "C:\Users\audacia\Desktop\fortran\fileInput\"
    
    type :: scarico_titoli
        integer :: compagnia
        integer :: agenzia
        integer :: intermediario
        character(len=lunghezzaSingoloCampoShort) :: produttore
        integer :: data_giornale_cassa
        integer :: pagina_giornale_cassa
        integer :: linea_giornale_cassa
        integer :: tipo_operazione
        integer :: classe_polizza
        integer :: numero_polizza
        integer :: numero_appendice
        character(len=lunghezzaSingoloCampoMiddle) :: contraente
        integer :: data_inizio
        integer :: data_scadenza
        character(len=lunghezzaSingoloCampoShort) :: frequenza_pagamento
        real(kind = maxPrecisione) :: premio_netto
        real(kind = maxPrecisione) :: premio_lordo
        integer :: data_riscossione
        integer :: data_pagamento
        real(kind = maxPrecisione) :: premio_pagato
        real(kind = maxPrecisione) :: commissioni
        real(kind = maxPrecisione) :: imposte_totali
        real(kind = maxPrecisione) :: ssn
        real(kind = maxPrecisione) :: rca_commissioni
        real(kind = maxPrecisione) :: commissioni_cvt
        real(kind = maxPrecisione) :: antiraket_tasse
        character(len=lunghezzaSingoloCampoMiddle) :: residenza
        real(kind = maxPrecisione) :: prov_tasse
        character(len=lunghezzaSingoloCampoShort) :: sigla_targa
        character(len=lunghezzaSingoloCampoShort) :: nr_targa
        integer :: tipo_veicolo
        character(len=lunghezzaSingoloCampo) :: modello
        character(len=lunghezzaSingoloCampoShort) :: alimentazione
        character(len=lunghezzaSingoloCampoShort) :: uso
        integer :: pieno_carico
        integer :: data_immatricolazione
        integer :: eta_conducente
        character(len=lunghezzaSingoloCampoShort) :: tipo_quietanzamento
    end type scarico_titoli
    
    
    type :: scarico_dettagliato_titoli
        integer :: data_foglio_cassa
        integer :: numero_foglio_cassa
        integer :: riga_foglio_cassa
        integer :: operation_type
        integer :: data_incasso
        character(len=lunghezzaSingoloCampoMiddle) :: contraente
        character(len=lunghezzaSingoloCampoMiddle) :: cod_fis_par_iva
        integer :: numero_polizza
        integer :: numero_appendice
        integer :: data_decorrenza
        character(len=lunghezzaSingoloCampoShort) :: frazionamento
        integer :: ramo
        character(len=lunghezzaSingoloCampoShort) :: pdgar_codgaranzia
        real(kind = maxPrecisione) :: pdgar_netto1rata
        real(kind = maxPrecisione) :: pdgar_tasse1rata
        real(kind = maxPrecisione) :: pdgar_abbuono1rata
        real(kind = maxPrecisione) :: contributo_ssn
        real(kind = maxPrecisione) :: antiraket
        character(len=lunghezzaSingoloCampoShort) :: provincia_di_residenza
        real(kind = maxPrecisione) :: imposte_provinciali
        character(len=lunghezzaSingoloCampoShort) :: sigla_targa
        character(len=lunghezzaSingoloCampoShort) :: numero_targa
    end type scarico_dettagliato_titoli    
    

END MODULE a_costanti_tipi_module
