!     
! File:   input_file.f95
! Author: audacia
!
! Created on 25 aprile 2020, 23.32
!

MODULE input_file
    
    implicit none
    
    type :: met_record
        integer :: colonna1
        integer :: colonna2
        character(len=32) :: str1
    end type met_record
  
    contains 
    
    !funzione che legge lo scarico dettagliato e lo carica in memoria 
    function readScaritoTitoliIntoArray()
        implicit none
        integer :: readScaritoTitoliIntoArray
        type(met_record), dimension(3) :: weather_reports

        integer :: i, ios, fu,j

        !lettura di un file csv di cui non conosco il numero di record
        print*, "non conosco il numero di record del file csv, lo apro e leggo in modo differente"
        open(FILE="C:\Users\audacia\Desktop\fortran\fileInput\fileInput2.csv", &
            &  newunit=fu, STATUS="OLD", ACTION="READ",FORM="FORMATTED",POSITION="REWIND")
        print*, "leggo e scrivo il programma"
        i=1
        do
            print*, "sono nel ciclo di lettura"
            read(fu,*,IOSTAT=ios) weather_reports(i)
            if(ios==0) then ! Nessun problema
                print*, "letto una riga"
                print*, i,">",weather_reports(i),"<"
                i = i+1
            else ! Problemi in lettura
                read(*,*) j                
                print*, "termino la lettura problemi"
                exit
            end if
        end do
        
        
        print*, "Stampa del vettore"
        do i = 1, 3
            print*, weather_reports(i)
        end do
        close(fu)
        read(*,*) j
        readScaritoTitoliIntoArray = 0
        
    end function readScaritoTitoliIntoArray

END MODULE input_file
