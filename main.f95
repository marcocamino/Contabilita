!     
! File:   main.f95
! Author: audacia
!
! Created on 21 aprile 2020, 21.55
!

program main 
    use a_costanti_tipi_module
    use costanti_menu   
    use input_file
    
    
    implicit none
    
    ! da togliere i
    integer :: scelta,i,dimensioneVettoreScaricoTitoli
    CHARACTER(len=100) :: pathNameScaricoTitoli
    
    !puntatore al vettore contenente i titoli dello scarico decadale
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    
    print*, "Questo programma effettua un controllo sulla quadratura degli scarichi"
    
    !leggo il nome del file dello scarico titoli
    print*, "La directory di lavoro è:    C:\Users\audacia\Desktop\fortran"
    write(*,'(a41)', advance='no')  "Digita il nome del file scarico titoli: "
    read(*,*) pathNameScaricoTitoli
    
    dimensioneVettoreScaricoTitoli = readScaritoTitoliIntoArray(vettoreScaricoTitoli)

    if (dimensioneVettoreScaricoTitoli > 0) then
        scelta = printAndChoice()

        do while (scelta /= maxScelta)

            if (scelta == 1) then 
                print*, "opzione 1"
            else if (scelta == 2) then 
                print*, "opzione 2"
            else if (scelta == 3) then 
                print*, "opzione 3"
            else 
                print*, "opzione 4" 
            end if

            read(*,*) scelta
            scelta = printAndChoice()  
        end do
    
    else
        !call system('clear')
        print*, "Non è stata allocata la memoria per importare i file dei titoli, l'elaborazione non può proseguire."
    end if
    
    
    
    
    
    !solo esempi  da cancellare
    call show_consts()
    
    Print*, "e raised to the power of 2.0 = ", ePowerx(2.0)
 
    Print*, "e raised to the power of 2.0 = ", quadrato(2.0)
    
    WRITE(*,'("Inserisci il valore di m: ")',ADVANCE='NO')
    READ(*,'(I5)') scelta
    
    !ciclo temporaneo per verificare che i dati siano stati letti dal file 
        do i = 1, dimensioneVettoreScaricoTitoli 
            !read(18,*) buffer
            print*, i,">",vettoreScaricoTitoli(i),"<"
        end do
        
        
        
        
        
    print*, "Programma terminato"
    !call system('clear')  
    contains

end program main 

