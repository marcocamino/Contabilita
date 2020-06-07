!     
! File:   main.f95
! Author: audacia
!
! Created on 21 aprile 2020, 21.55
! versione 1.1 del 20200606
!

program main 
    use a_costanti_tipi_module
    use b_menu   
    use input_file
    use controllo_quadratura_scarichi
       
    implicit none
    
    integer :: scelta
    CHARACTER(len=100) :: nameFileScaricoTitoli
    CHARACTER(len=100) :: nameFileScaricoTitoliDettagliato
    
    !puntatore al vettore contenente i titoli dello scarico decadale
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    integer :: dimensioneVettoreScaricoTitoli = 0
    
    !puntatore al vettore contenente i titoli dettagliati per garanzia 
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    integer :: dimensioneVettoreScaricoDettagliatoTitoli = 0
       
    print*, "Questo programma effettua un controllo sulla quadratura degli scarichi"
    
    !leggo il nome del file dello scarico titoli
    print*, "La directory di lavoro è: " ,pathNameWorkingDirectory
    write(*,'(a41)', advance='no')  "Digita il nome del file scarico titoli: "
    read(*,*) nameFileScaricoTitoli
    
    !versione temporanea per sviluppo
    nameFileScaricoTitoli = pathNameWorkingDirectory(1:(LEN_TRIM(pathNameWorkingDirectory)))//&
                                                    &"ScaricoTitoloCassa_20200401_20200430.csv"
    print*, "Il path name completo è: " ,nameFileScaricoTitoli
    
    !routine che legge il file dei titoli e li memorizza in un vettore
    call readScaritoTitoliWriteIntoArray(vettoreScaricoTitoli,dimensioneVettoreScaricoTitoli,nameFileScaricoTitoli)
    
    
    nameFileScaricoTitoliDettagliato = pathNameWorkingDirectory(1:(LEN_TRIM(pathNameWorkingDirectory)))//&
                                                    &"ScaricoDettaglioTitoliTasse_20200401_20200430.csv"
                                                    
    !routine che legge il file contenente i titoli dettagliati per garanzia e memorizza in un array
    call readScaritoTitoliDettagliatiWriteIntoArray(vettoreScaricoDettagliatoTitoli,dimensioneVettoreScaricoDettagliatoTitoli,&
                                                    &nameFileScaricoTitoliDettagliato)

    !se entrambi i file sono stati memorizzati negli opportuni array procedo con l'elaborazione                                               
    if ((dimensioneVettoreScaricoTitoli > 0) .and. (dimensioneVettoreScaricoDettagliatoTitoli > 0)) then
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
                print*, "opzione 4 in progress..." 
            end if
            scelta = printAndChoice()  
        end do
    
    else
        call system('clear')
        print*, "Non è stata allocata la memoria per importare i file dei titoli, l'elaborazione non può proseguire."
    end if
          
    print*, "Programma terminato" 
    contains

end program main 

