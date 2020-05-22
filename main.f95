!     
! File:   main.f95
! Author: audacia
!
! Created on 21 aprile 2020, 21.55
!

program main 
    use a_costanti_tipi_module
    use b_menu   
    use input_file
    use controllo_quadratura_scarichi
       
    implicit none
    
    ! da togliere i
    integer :: scelta,i
    CHARACTER(len=100) :: nameFileScaricoTitoli
    CHARACTER(len=100) :: nameFileScaricoTitoliDettagliato
    
    !puntatore al vettore contenente i titoli dello scarico decadale
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    integer :: dimensioneVettoreScaricoTitoli
    
    !puntatore al vettore contenente i titoli dettagliati per garanzia 
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    integer :: dimensioneVettoreScaricoDettagliatoTitoli
       
    print*, "Questo programma effettua un controllo sulla quadratura degli scarichi"
    
    !leggo il nome del file dello scarico titoli
    print*, "La directory di lavoro è: " ,pathNameWorkingDirectory
    write(*,'(a41)', advance='no')  "Digita il nome del file scarico titoli: "
    read(*,*) nameFileScaricoTitoli
    
    !versione temoranea
    nameFileScaricoTitoli = pathNameWorkingDirectory(1:(LEN_TRIM(pathNameWorkingDirectory)))//&
                                                    &"ScaricoTitoloCassa_20200401_20200430.csv"
    print*, "Il path name completo è: " ,nameFileScaricoTitoli
    
    !versione con funzioni  - funzionante
    !dimensioneVettoreScaricoTitoli = readScaritoTitoliIntoArray(vettoreScaricoTitoli)
    
    !versione con subroutine
    call readScaritoTitoliWriteIntoArray(vettoreScaricoTitoli,dimensioneVettoreScaricoTitoli,nameFileScaricoTitoli)
    
    
    nameFileScaricoTitoliDettagliato = pathNameWorkingDirectory(1:(LEN_TRIM(pathNameWorkingDirectory)))//&
                                                    &"ScaricoDettaglioTitoliTasse_20200401_20200430.csv"
    !lettura del file contenente i titoli dettagliati per garanzia e scrittura del file in un array
    call readScaritoTitoliDettagliatiWriteIntoArray(vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli,&
                                                    &nameFileScaricoTitoliDettagliato)

    if (dimensioneVettoreScaricoTitoli > 0) then
        scelta = printAndChoice()

        do while (scelta /= maxScelta)

            if (scelta == 1) then 
                call controllo_basi_imponibili(vettoreScaricoTitoli,dimensioneVettoreScaricoTitoli,&
                    &vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli)
            else if (scelta == 2) then 
                call verifica_quadratura_tasse(vettoreScaricoTitoli,dimensioneVettoreScaricoTitoli,&
                    &vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli)
            else if (scelta == 3) then 
                call verifica_quadratura_scarichi(vettoreScaricoTitoli,dimensioneVettoreScaricoTitoli,&
                    &vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli)
            else 
                print*, "opzione 4" 
            end if

            scelta = printAndChoice()  
        end do
    
    else
        !call system('clear')
        print*, "Non è stata allocata la memoria per importare i file dei titoli, l'elaborazione non può proseguire."
    end if
     
    !solo esempi  da cancellare
    
    WRITE(*,'("Inserisci il valore di m: ")',ADVANCE='NO')
    READ(*,'(I5)') scelta
    
    !ciclo temporaneo per verificare che i dati siano stati letti dal file 
        do i = 1, dimensioneVettoreScaricoDettagliatoTitoli 
            !read(18,*) buffer
            print*, i,">",vettoreScaricoDettagliatoTitoli(i),"<"
        end do
          
    print*, "Programma terminato"
    !call system('clear')  
    contains

end program main 

