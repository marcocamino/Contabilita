!     
! File:   input_file.f95
! Author: audacia
!
! Created on 25 aprile 2020, 23.32
!

MODULE input_file
    use a_costanti_tipi_module
    implicit none
    
!    type :: met_record
!        integer :: colonna1
!        integer :: colonna2
!        character(len=32) :: str1
!    end type met_record
  
    contains 
    
    !funzione che legge lo scarico dettagliato e lo carica in memoria 
!    function readScaritoTitoliIntoArray()
!        implicit none
!        integer :: readScaritoTitoliIntoArray
!        type(met_record), dimension(3) :: weather_reports
!
!        integer :: i, ios, fu,j
!
!        !lettura di un file csv di cui non conosco il numero di record
!        print*, "non conosco il numero di record del file csv, lo apro e leggo in modo differente"
!        open(FILE="C:\Users\audacia\Desktop\fortran\fileInput\fileInput2.csv", &
!            &  newunit=fu, STATUS="OLD", ACTION="READ",FORM="FORMATTED",POSITION="REWIND")
!        print*, "leggo e scrivo il programma"
!        i=1
!        do
!            print*, "sono nel ciclo di lettura"
!            read(fu,*,IOSTAT=ios) weather_reports(i)
!            if(ios==0) then ! Nessun problema
!                print*, "letto una riga"
!                print*, i,">",weather_reports(i),"<"
!                i = i+1
!            else ! Problemi in lettura
!                read(*,*) j                
!                print*, "termino la lettura problemi"
!                exit
!            end if
!        end do
!        
!        
!        print*, "Stampa del vettore"
!        do i = 1, 3
!            print*, weather_reports(i)
!        end do
!        close(fu)
!        read(*,*) j
!        readScaritoTitoliIntoArray = 0
!        
!    end function readScaritoTitoliIntoArray
    
    
!Questa funzione legge il file dello scarico titoli e lo salva in un array.
    function readScaritoTitoliIntoArray(vettoreScaricoTitoli)
        implicit none
        
        !puntatore al vettore contenente i titoli letti dal file
        type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
        !numero di titoli presenti nel file e dimensione del vettore
        integer :: readScaritoTitoliIntoArray 
        
        
        type(scarico_titoli), dimension(1) :: titolo
        character(len=300) :: buffer

        integer :: i, ios, fu,j,stato,numRighe

        !lettura di un file csv per determianre il numero di record di cui è costituito.
        print*, "Leggo il file csv per determinare il numero di record."
        open(FILE="C:\Users\audacia\Desktop\fortran\fileInput\ScaricoTitoloCassa_20181201_20181231.csv", &
            &  newunit=fu, STATUS="OLD", ACTION="READ",FORM="FORMATTED",POSITION="REWIND")
        !print*, "leggo e scrivo il programma"
        i=0
        do
            read(fu,"(A)",IOSTAT=ios) buffer
            if(ios==0) then 
                ! Nessun problema
                i = i+1
                !print*, i,">",buffer,"<"
            else 
                ! Problemi in lettura oppure fine file
                !print*, "Fine file: ", ios
                !read(*,*) j                
                exit
            end if
        end do
        
        !chiudo il file
        close(fu)
        
        numRighe = i
        print*, "Il file è composto da: ", numRighe, " righe."
        !read(*,*) j
        
        print*, "Alloco la memoria "
        ALLOCATE(vettoreScaricoTitoli(numRighe),STAT=stato)
        IF (stato == 0 ) THEN
            Print*, "Ho allocato la memoria dinamica per i titoli letti nel file."
            !read(*,*) j
            
            !lettura di un file csv, ma bisogna sapere prima il numero di record
            PRINT*, "Leggo il  file csv, e scrivo i records in memoria"

            open(unit=18, FILE="C:\Users\audacia\Desktop\fortran\fileInput\ScaricoTitoloCassa_20181201_20181231.csv", &
                & status='old' , access ='sequential',form='formatted')

            do i = 1, numRighe 
                read(18,"(A)",IOSTAT=ios) buffer
                vettoreScaricoTitoli(i)%compagnia = i
                print*, i,">",buffer,"<"
            end do
            close(unit=18)

            PRINT*, "Terminato di scrivere nel vettore i titoli presenti nel file."
            read(*,*) j
        else 
            numRighe = 0
            
        end if
        
        
!        !lettura di un file csv, ma bisogna sapere prima il numero di record
!        PRINT*, "Leggo il  file csv, e scrivo i records in memoria"
!
!        open(unit=18, FILE="C:\Users\audacia\Desktop\fortran\fileInput\ScaricoTitoloCassa_20181201_20181231.csv", status='old' , &
!         &  access ='sequential',form='formatted')
!
!        do i = 1, numRighe 
!            read(18,"(A)",IOSTAT=ios) buffer
!            vettoreScaricoTitoli(i)%compagnia = i
!            print*, i,">",buffer,"<"
!        end do
!        close(unit=18)
!        
!        PRINT*, "Terminato di scrivere nel vettore i titoli presenti nel file."
!        read(*,*) j
        
        readScaritoTitoliIntoArray = numRighe
        
    end function readScaritoTitoliIntoArray

END MODULE input_file
