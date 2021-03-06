!     
! File:   input_file.f95
! Author: audacia
!
! Created on 25 aprile 2020, 23.32
! versione 1.1 del 20200606

MODULE input_file
    use a_costanti_tipi_module
    implicit none
  
    contains 
    
    subroutine readScaritoTitoliDettagliatiWriteIntoArray(vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli&
        &, nameFileScaricoTitoliDettagliato)
        implicit none
        
        !puntatore al vettore contenente i titoli letti dal file
        type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
        !numero di titoli presenti nel file e dimensione del vettore
        integer :: dimensioneVettoreScaricoDettagliatoTitoli 
        !path name del file contenente i titoli dettagliati per garanzia.
        CHARACTER(len=lunghezzaPathName) :: nameFileScaricoTitoliDettagliato
                
        character(len=lunghezzaSingolaRigaCSV) :: buffer
        integer :: i=0, ios, fu,j,stato,numRighe
        character(len=1) :: input
        
        !lettura di un file csv per determianre il numero di record di cui è costituito.
        open(FILE=nameFileScaricoTitoliDettagliato, newunit=fu, STATUS="OLD", ACTION="READ",FORM="FORMATTED",POSITION="REWIND")

        do
            read(fu,"(A)",IOSTAT=ios) buffer
            if(ios==0) then 
                ! Nessun problema
                i = i+1
            else 
                ! Problemi in lettura oppure fine file               
                exit
            end if
        end do

        close(fu)
        
        numRighe = i 
        print*, "Il file titoli dettagliato per garanzie è composto da: ", numRighe, " righe."

        print*, "Alloco la memoria "
        ALLOCATE(vettoreScaricoDettagliatoTitoli(numRighe ),STAT=stato)
        IF (stato == 0 ) THEN
            Print*, "Ho allocato la memoria dinamica per i titoli letti nel file."
            
            !lettura di un file csv, ma bisogna sapere prima il numero di record
            print*, "Leggo il  file csv, e scrivo i records in memoria"
            open(unit=unitNumber, FILE=nameFileScaricoTitoliDettagliato, status='old' , access ='sequential',form='formatted')

            do i = 1, (numRighe )
                read(unitNumber,"(A)",IOSTAT=ios) buffer
                if(i > num_righe_intestazione_titoli_dettagliato) then
                    call copia_record_dettagliato(buffer, vettoreScaricoDettagliatoTitoli,( i - &
                                    &num_righe_intestazione_titoli_dettagliato))
                endif
            end do
            close(unit=unitNumber)

            print*, "Terminato di scrivere nel vettore i titoli dettagliati presenti nel file."
            write(*,'(a14)', advance='no')  "Premi invio. "
            read(*, '(A)') input
            dimensioneVettoreScaricoDettagliatoTitoli = numRighe - num_righe_intestazione_titoli_dettagliato
        else 
            dimensioneVettoreScaricoDettagliatoTitoli = 0
        endif
                  
    end subroutine
    
    
    !questa routine legge il file dello scarico titoli e lo memorizza in un vettore
    subroutine readScaritoTitoliWriteIntoArray (vettoreScaricoTitoli, lunghezzaVettoreScaricoTitoli,nameFileScaricoTitoli)
        implicit none
        
        !puntatore al vettore contenente i titoli letti dal file
        type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
        !numero di titoli presenti nel file e dimensione del vettore
        integer :: lunghezzaVettoreScaricoTitoli
        CHARACTER(len=lunghezzaPathName) :: nameFileScaricoTitoli
               
        type(scarico_titoli), dimension(1) :: titolo
        character(len=lunghezzaSingolaRigaCSV) :: buffer
        character(len=1) :: inputCarattere

        integer :: i=0, ios, fu, stato, numRighe

        !leggo il file csv per determianre il numero di record di cui è costituito.
        open(FILE=nameFileScaricoTitoli, newunit=fu, STATUS="OLD", ACTION="READ",FORM="FORMATTED",POSITION="REWIND")

        do
            read(fu,"(A)",IOSTAT=ios) buffer
            if(ios==0) then 
                ! Nessun problema
                i = i+1
            else 
                ! Problemi in lettura oppure fine file           
                exit
            end if
        end do
        
        close(fu)
        
        numRighe = i 
        print*, "Il file è composto da: ", numRighe, " righe, comprese le righe di intestazione e chiusura."
        ALLOCATE(vettoreScaricoTitoli(numRighe - num_righe_intestazione_chiusura_titoli),STAT=stato)
        IF (stato == 0 ) THEN
            Print*, "Ho allocato la memoria dinamica per i titoli contenuti nel file."
            
            !lettura di un file csv, ma bisogna sapere prima il numero di record
            print*, "Leggo il  file csv, e scrivo i records nel vettore"
            open(unit=unitNumber, FILE=nameFileScaricoTitoli, status='old' , access ='sequential',form='formatted')

            do i = 1, (numRighe - num_righe_chiusura_titoli)
                read(unitNumber,"(A)",IOSTAT=ios) buffer
                if(i > num_righe_intestazione_titoli) then
                    call copia_record(buffer, vettoreScaricoTitoli,( i - num_righe_intestazione_titoli))
                endif
            end do
            close(unit=unitNumber)

            print*, "Terminato di scrivere nel vettore i titoli presenti nel file."            
            numRighe = numRighe - num_righe_intestazione_chiusura_titoli
        else 
            numRighe = 0 
            print*, "Non è stato possibile allocare la memoria."
        endif
        write(*,'(a14)', advance='no')  "Premi invio. "
        read(*, '(A)') inputCarattere
        lunghezzaVettoreScaricoTitoli = numRighe
              
    end subroutine
     

    !routine che copia il record del file titoli dettagliati per garanzia nel vettore contente i titoli dettagliati per garanzia
    subroutine copia_record_dettagliato(record_stringa, vettoreScaricoDettagliatoTitoli, posizione)
        implicit none

        character(len=lunghezzaSingolaRigaCSV) :: record_stringa
        type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
        integer :: posizione
        
        character(len=lunghezzaSingoloCampo) :: campo_stringa
        integer :: posizione_separatore, campo_intero
        real(kind = maxPrecisione) :: campo_reale
        
        !separo il valore della data foglio cassa e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%data_foglio_cassa = campo_intero

        !separo il valore del numero foglio cassa e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%numero_foglio_cassa = campo_intero
 
        !separo il valore della riga foglio cassa e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%riga_foglio_cassa = campo_intero
        
        !separo il valore del tipo operazione e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%operation_type = campo_intero 
        
        !separo il valore della data incasso e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%data_incasso = campo_intero   
        
        !contraente
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%contraente = campo_stringa  
        
        !cod_fis_par_iva
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%cod_fis_par_iva = campo_stringa  
        
        !separo il valore del numero polizza e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%numero_polizza = campo_intero   
        
        !separo il valore del numero appendice e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%numero_appendice = campo_intero   
        
        !separo il valore della data decorrenza e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%data_decorrenza = campo_intero 
        
        !frazionamento
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%frazionamento = campo_stringa 
        
        !separo il valore del ramo e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreScaricoDettagliatoTitoli(posizione)%ramo = campo_intero 
        
        !pdgar_codgaranzia
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%pdgar_codgaranzia = campo_stringa   
        
        !pdgar_netto1rata
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%pdgar_netto1rata = campo_reale 
        
        !pdgar_tasse1rata
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%pdgar_tasse1rata = campo_reale
        
        !pdgar_abbuono1rata
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%pdgar_abbuono1rata = campo_reale
        
        !contributo_ssn
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%contributo_ssn = campo_reale
        
        !antiraket
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%antiraket = campo_reale    
        
        !provincia_di_residenza
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%provincia_di_residenza = campo_stringa
        
        !imposte_provinciali
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreScaricoDettagliatoTitoli(posizione)%imposte_provinciali = campo_reale 
        
        !sigla_targa
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%sigla_targa = campo_stringa 
        
        !numero_targa
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreScaricoDettagliatoTitoli(posizione)%numero_targa = campo_stringa 
        
    end subroutine
    
    
    
    !Routine che copia il record del file csv nel vettore contenente i titoli dello scarico decadale
    subroutine copia_record(record_stringa, vettoreRecordScaricoTitoli, posizione)
        implicit none
        
        character(len=lunghezzaSingolaRigaCSV) :: record_stringa
        type(scarico_titoli), POINTER :: vettoreRecordScaricoTitoli(:)
        integer :: posizione
        
        character(len=lunghezzaSingoloCampo) :: campo_stringa
        integer :: posizione_separatore, campo_intero
        real(kind = maxPrecisione) :: campo_reale
        
        
        !separo il valore della compagnia e lo scrivo nel rispettivo campo del vettore
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%compagnia = campo_intero
        
        !agenzia      
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%agenzia = campo_intero

        !intermediario
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%intermediario = campo_intero  
        
        !produttore
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%produttore = campo_stringa   
        
        !data_giornale_cassa
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_giornale_cassa = campo_intero 
        
        !pagina_giornale_cassa
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%pagina_giornale_cassa = campo_intero 
        
        !linea_giornale_cassa
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%linea_giornale_cassa = campo_intero 
        
        !tipo_operazione
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%tipo_operazione = campo_intero 
        
        !classe_polizza
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%classe_polizza = campo_intero 

        !numero_polizza
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%numero_polizza = campo_intero 
        
        !numero_appendice
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%numero_appendice = campo_intero 

        !contraente
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%contraente = campo_stringa    
        
        !data_inizio
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_inizio = campo_intero    
        
        !data_scadenza
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_scadenza = campo_intero  

        !frequenza_pagamento
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%frequenza_pagamento = campo_stringa  
        
        !premio_netto
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%premio_netto = campo_reale   

        !premio_lordo
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%premio_lordo = campo_reale 
        
        !data_riscossione
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_riscossione = campo_intero 
        
        !data_pagamento
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_pagamento = campo_intero 
        
        !premio_pagato
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%premio_pagato = campo_reale 
        
        !commissioni
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%commissioni = campo_reale  
        
        !imposta_totali
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%imposte_totali = campo_reale     
        
        !ssn
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%ssn = campo_reale  
        
        !rca_commissioni
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%rca_commissioni = campo_reale  
        
        !commissioni_cvt
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%commissioni_cvt = campo_reale  
        
        !antiraket_tasse
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%antiraket_tasse = campo_reale   
        
        !residenza
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%residenza = campo_stringa 

        !prov_tasse
        call dammi_prossimo_campo_reale(record_stringa,campo_reale)
        vettoreRecordScaricoTitoli(posizione)%prov_tasse = campo_reale 
        
        !sigla_targa
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%sigla_targa = campo_stringa 
        
        !nr_targa
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%nr_targa = campo_stringa 
        
        !tipo_veicolo
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%tipo_veicolo = campo_intero 
        
        !modello
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%modello = campo_stringa 
        
        !alimentazione
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%alimentazione = campo_stringa 
        
        !uso
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%uso = campo_stringa 
        
        !pieno_carico
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%pieno_carico = campo_intero 
        
        !data_immatricolazione
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%data_immatricolazione = campo_intero 
        
        !eta_conducente
        call dammi_prossimo_campo_intero(record_stringa,campo_intero)
        vettoreRecordScaricoTitoli(posizione)%eta_conducente = campo_intero  
        
        !tipo_quietanzamento
        call dammi_prossimo_campo_stringa(record_stringa,campo_stringa)
        vettoreRecordScaricoTitoli(posizione)%tipo_quietanzamento = campo_stringa 
        
    end subroutine copia_record
    
    
    !routine che data una stringa con i valori separati da ; restituisce il primo da sinistra di tipo integer e la stringa iniziale 
    !priva del primo valore a sinistra 
    subroutine dammi_prossimo_campo_intero(stringa, primo_campo_intero)
        implicit none
        character(len=lunghezzaSingolaRigaCSV) :: stringa
        integer :: primo_campo_intero
        
        character(len=lunghezzaSingoloCampo) :: primo_campo_stringa        
        integer :: posizione_del_separatore
      
        posizione_del_separatore = index(stringa, ";")
        primo_campo_stringa = stringa(:posizione_del_separatore - 1)
        primo_campo_stringa = trim(primo_campo_stringa)
        if( primo_campo_stringa == "") then
            primo_campo_intero = 0
        else
            read(primo_campo_stringa , *) primo_campo_intero
        endif
        stringa = stringa(posizione_del_separatore + 1:)
    end subroutine

    !routine che data una stringa con i valori separati da ; restituisce il primo da sinistra di tipo reale e la stringa iniziale 
    !priva del primo valore a sinistra
    subroutine dammi_prossimo_campo_reale(stringa, primo_campo_reale)
        implicit none
        character(len=lunghezzaSingolaRigaCSV) :: stringa
        real(kind = maxPrecisione) :: primo_campo_reale
        
        character(len=lunghezzaSingoloCampo) :: primo_campo_stringa        
        integer :: posizione_del_separatore
      
        posizione_del_separatore = index(stringa, ";")
        primo_campo_stringa = stringa(:posizione_del_separatore - 1)
        primo_campo_stringa = trim(primo_campo_stringa)
        if( primo_campo_stringa == "") then
            primo_campo_reale = 0
        else
            read(primo_campo_stringa , *) primo_campo_reale
        endif
        stringa = stringa(posizione_del_separatore + 1:)
    end subroutine   
    
        
    !routine che data una stringa con i valori separati da ; restituisce il primo da sinistra di tipo stringa e la stringa iniziale 
    !priva del primo valore a sinistra    
    subroutine dammi_prossimo_campo_stringa(stringa, primo_campo_stringa)
        implicit none
        character(len=lunghezzaSingolaRigaCSV) :: stringa
        character(len=lunghezzaSingoloCampo) :: primo_campo_stringa
               
        integer :: posizione_del_separatore
      
        posizione_del_separatore = index(stringa, ";")
        primo_campo_stringa = stringa(:posizione_del_separatore - 1)
        primo_campo_stringa = trim(primo_campo_stringa)
        stringa = stringa(posizione_del_separatore + 1:)
    end subroutine
    
END MODULE input_file
