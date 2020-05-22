!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!     
! File:   controllo_quadratura_scarichi.f95
! Author: audacia
!
! Created on 18 maggio 2020, 23.15
!

MODULE controllo_quadratura_scarichi
    use a_costanti_tipi_module
    implicit none 
contains 

subroutine controllo_basi_imponibili(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    integer :: i,j
    real :: base_imponibile
    
    !ciclo sui titoli dello scarico decadale
    do i = 1, dimensioneVettoreScaricoTitoli
        
        print*, "________________________________________________________________________________________"
        print*, "titoli"
        print*, vettoreScaricoTitoli(i)%numero_polizza,vettoreScaricoTitoli(i)%tipo_operazione,&
            &vettoreScaricoTitoli(i)%numero_appendice
        
        !aggrego lo scarico dettagliato per confrontarlo con scarico decadale
        base_imponibile = 0
        do J=1, dimensioneVettoreScaricoDettagliatoTitoli
            if((vettoreScaricoTitoli(i)%numero_polizza == vettoreScaricoDettagliatoTitoli(j)%numero_polizza) .and. &
               &(vettoreScaricoTitoli(i)%tipo_operazione == vettoreScaricoDettagliatoTitoli(j)%operation_type).and. &
               &(vettoreScaricoTitoli(i)%numero_appendice == vettoreScaricoDettagliatoTitoli(j)%numero_appendice))then 
                base_imponibile = base_imponibile + vettoreScaricoDettagliatoTitoli(j)%pdgar_netto1rata &
                & - vettoreScaricoDettagliatoTitoli(j)%pdgar_abbuono1rata
                print*, "titoli dettagliati"
                print*,  vettoreScaricoDettagliatoTitoli(j)%numero_polizza, vettoreScaricoDettagliatoTitoli(j)%operation_type, &
                    &vettoreScaricoDettagliatoTitoli(j)%numero_appendice, vettoreScaricoDettagliatoTitoli(j)%pdgar_netto1rata
                print*, "+++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            end if
        
        end do
        
        !controllo sulla quadratura
        if (abs(base_imponibile - vettoreScaricoTitoli(i)%premio_netto) > 0.01) then
            print*, "squadratura", vettoreScaricoTitoli(i)%numero_polizza, vettoreScaricoTitoli(i)%tipo_operazione
            print*, vettoreScaricoTitoli(i)%numero_appendice, abs(base_imponibile - vettoreScaricoTitoli(i)%premio_netto)
            print*, base_imponibile, vettoreScaricoTitoli(i)%premio_netto
            print*, "************************************************************"
        else 
            print*, i, "ok", abs(base_imponibile - vettoreScaricoTitoli(i)%premio_netto), base_imponibile, &
                &vettoreScaricoTitoli(i)%premio_netto
            print*, "************************************************************"
        end if
    end do 
    read(*,*) i
    
end subroutine

subroutine verifica_quadratura_tasse(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    integer :: i,j
    real :: tasse_totali_bdx, tasse_1_rata_dettagliato, differenza
    
    print*, "Sono nella rutine che ricerca la squadratura delle tasse tra i due scarichi"
    
    !ciclo sui titoli dello scarico decadale
    do i = 1, dimensioneVettoreScaricoTitoli
        
!        print*, "_________________________________________________________________________________________"
!        print*, "Polizza : ", vettoreScaricoTitoli(i)%numero_polizza
!        print*, "Operazione", vettoreScaricoTitoli(i)%tipo_operazione
!        print*, "Imposte totali", vettoreScaricoTitoli(i)%imposte_totali
!        print*, "SSN", vettoreScaricoTitoli(i)%ssn
!        print*, "Antiraket", vettoreScaricoTitoli(i)%antiraket_tasse
!        print*, "Tasse provinciali", vettoreScaricoTitoli(i)%prov_tasse

        tasse_totali_bdx = vettoreScaricoTitoli(i)%imposte_totali + vettoreScaricoTitoli(i)%ssn + &
          & vettoreScaricoTitoli(i)%antiraket_tasse + vettoreScaricoTitoli(i)%prov_tasse
          
        !ciclo sullo scarico titoli dettaglati per garanzia, e sommo le garanzie per verificare la quadratura 
        tasse_1_rata_dettagliato = 0  
        do J=1, dimensioneVettoreScaricoDettagliatoTitoli
            if((vettoreScaricoTitoli(i)%numero_polizza == vettoreScaricoDettagliatoTitoli(j)%numero_polizza) .and. &
                &(vettoreScaricoTitoli(i)%tipo_operazione == vettoreScaricoDettagliatoTitoli(j)%operation_type)) then
                tasse_1_rata_dettagliato = tasse_1_rata_dettagliato + vettoreScaricoDettagliatoTitoli(j)%pdgar_tasse1rata
            end if
        end do
        
        differenza = ABS(tasse_1_rata_dettagliato - tasse_totali_bdx)
        if (differenza > 0.01) then
            print*, "_________________________________________________________________________________________"
            print*, "Squadratura: ", differenza, "numero polizza: ", vettoreScaricoTitoli(i)%numero_polizza
            print*, "tasse totali bourderaux: ", tasse_totali_bdx, "tasse totali dettaglato: ", tasse_1_rata_dettagliato
!        else 
!            print*, "OK "
        end if
    end do
    
    print*, "Ho terminato di effettuare la quadratura"
    read(*,*) i
    
    
end subroutine


!Questa procedura verifica la quadratura tra i due scarichi
subroutine verifica_quadratura_scarichi(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    
    integer :: i,j
    real :: totale_netto1rata = 0, totale_tasse1rata = 0, totale_abbuono1rata = 0, totale_contributo_ssn = 0, &
        & totale_antiraket = 0 ,totale_imposte_provinciali = 0, premio_netto_dettagliato = 0,  &
        & totale_premio_netto_titoli = 0, differenza_premi_netti, totale_premio_lordo_titoli = 0, &
        & totale_tasse_titoli = 0, imposte_totali_titoli = 0, totale_ssn_titoli = 0, totale_antiraket_titoli = 0, &
        & totale_prov_tasse_titoli = 0, totale_imposte = 0
        
    !ciclo sullo scarico dettagliato e calcolo le somme totali del premio netto,tasse1rata, abbuono1rata,
    !ssn, antiraket, imposte provinciali
    print*, "Sono nella procedura di verifica quadratura tra i due scarichi"
    read(*,*) i
       
    do J=1, dimensioneVettoreScaricoDettagliatoTitoli
        
        totale_netto1rata = totale_netto1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_netto1rata
        totale_tasse1rata = totale_tasse1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_tasse1rata
        totale_abbuono1rata = totale_abbuono1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_abbuono1rata
        totale_contributo_ssn = totale_contributo_ssn + vettoreScaricoDettagliatoTitoli(j)%contributo_ssn 
        totale_antiraket = totale_antiraket + vettoreScaricoDettagliatoTitoli(j)%antiraket
        totale_imposte_provinciali = totale_imposte_provinciali + vettoreScaricoDettagliatoTitoli(j)%imposte_provinciali
     
    end do
    
    print*, "Premi netti dettagliato: ", totale_netto1rata
    print*, "Abbuono dettagliato: ", totale_abbuono1rata
    premio_netto_dettagliato = totale_netto1rata - totale_abbuono1rata
    
    
    !ciclo sullo scarico titoli aggragati 
    do j = 1, dimensioneVettoreScaricoTitoli
        totale_premio_netto_titoli = totale_premio_netto_titoli + vettoreScaricoTitoli(j)%premio_netto
        totale_premio_lordo_titoli = totale_premio_lordo_titoli + vettoreScaricoTitoli(j)%premio_lordo
        imposte_totali_titoli = imposte_totali_titoli + vettoreScaricoTitoli(j)%imposte_totali
        totale_ssn_titoli = totale_ssn_titoli + vettoreScaricoTitoli(j)%ssn
        totale_antiraket_titoli = totale_antiraket_titoli + vettoreScaricoTitoli(j)%antiraket_tasse
        totale_prov_tasse_titoli = totale_prov_tasse_titoli + vettoreScaricoTitoli(j)%prov_tasse
    end do
    
    totale_imposte = imposte_totali_titoli + totale_ssn_titoli + totale_antiraket_titoli + totale_prov_tasse_titoli
    print*, "Totale imposte titoli: ", totale_imposte
    
    differenza_premi_netti = ABS(premio_netto_dettagliato - totale_premio_netto_titoli)
    print*, "Premi netti titoli: ", totale_premio_netto_titoli
    print*, "Premi netti titoli dettagliati: ", premio_netto_dettagliato
    if (differenza_premi_netti < 0.01)then
        print*, "OK quadratura premi netti: ", differenza_premi_netti
    else 
        print*, "SQUADRATURA premi netti: ", differenza_premi_netti
    end if
    
    totale_tasse_titoli = totale_premio_lordo_titoli - totale_premio_netto_titoli
    print*, "Totale tasse titoli: ", totale_tasse_titoli
    print*, "Totale tasse titoli dettagliati: ", totale_tasse1rata
    
    
    print*, "Totale ssn titoli: ", totale_ssn_titoli
    print*, "Totale ssn titoli dettagliati: ", totale_contributo_ssn
    
    print*, "Totale antiraket titoli: ", totale_antiraket_titoli
    print*, "Totale antiraket titoli dettagliati: ", totale_antiraket
    
    print*, "Totale imposte provinciali titoli: ", totale_prov_tasse_titoli
    print*, "Totale imposte provinciali titoli dettagliati: ", totale_imposte_provinciali
    
    read(*,*) i
end subroutine 

END MODULE controllo_quadratura_scarichi
