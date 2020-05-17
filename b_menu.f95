!
! To change this license header, choose License Headers in Project Properties.
!     
! File:   costanti_menu.f95
! Author: audacia
!
! Created on 21 aprile 2020, 21.59
!

MODULE b_menu
    use a_costanti_tipi_module
    implicit none 

 
contains      
   
    function printAndChoice()
        implicit none
        integer :: printAndChoice
        call system('clear') 
        print*, "1) Controllo della doppia inclusione insiemistica tra i due scarichi. "
        print*, " "
        print*, "2) Controllo della quadrature contabile tra i due scarichi. "
        print*, " "
        print*, "3) Controllo che non vi siano operazioni 18 di un altro mese da scartare. "
        print*, " "
        print*, "4) calcolo tasse per regione. "
        print*, " "
        print*, "5) Termina. "
        print*, " "
        write(*,'(a65)', advance='no')  "Digita il numero corrispondente alla tua scelta e premi invio. "
        read(*,*) printAndChoice
        do while((printAndChoice < minScelta) .or. (printAndChoice > maxScelta))
            print*, " "
            print*, "Il valore inputato non appartiene al range permesso. "
            write(*,'(a65)', advance='no')  "Digita il numero corrispondente alla tua scelta e premi invio. "
            read(*,*) printAndChoice
            
        end do
    end function printAndChoice
    
END MODULE b_menu
