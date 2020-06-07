!     
! File:   controllo_quadratura_scarichi.f95
! Author: audacia
!
! Created on 18 maggio 2020, 23.15
! versione 1.1 del 20200606

MODULE controllo_quadratura_scarichi
    use a_costanti_tipi_module
    implicit none 
contains 

!routine che verifica la quadratura tra il premio netto del titolo aggregato con i premi netti dei titoli splittati per garanzia
subroutine controllo_basi_imponibili(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_titoli), pointer :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_dettagliato_titoli), pointer :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    
    integer :: i,j
    real(kind = maxPrecisione) :: premio_netto_dettagliato
    character(len=1) :: input
    logical :: quadratura 
    
    quadratura = .true.
    !ciclo sui titoli dello scarico decadale
    do i = 1, dimensioneVettoreScaricoTitoli
        
        !aggrego lo scarico dettagliato per confrontarlo con scarico decadale
        premio_netto_dettagliato = 0
        do J=1, dimensioneVettoreScaricoDettagliatoTitoli
            if((vettoreScaricoTitoli(i)%numero_polizza == vettoreScaricoDettagliatoTitoli(j)%numero_polizza) .and. &
               &(vettoreScaricoTitoli(i)%tipo_operazione == vettoreScaricoDettagliatoTitoli(j)%operation_type).and. &
               &(vettoreScaricoTitoli(i)%numero_appendice == vettoreScaricoDettagliatoTitoli(j)%numero_appendice))then 
                premio_netto_dettagliato = premio_netto_dettagliato + vettoreScaricoDettagliatoTitoli(j)%pdgar_netto1rata &
                & - vettoreScaricoDettagliatoTitoli(j)%pdgar_abbuono1rata
            end if        
        end do
        
        !controllo sulla quadratura
        if (abs(premio_netto_dettagliato - vettoreScaricoTitoli(i)%premio_netto) > 0.01) then
            print*, "squadratura"
            print*,"Num. polizza: ", vettoreScaricoTitoli(i)%numero_polizza, "operazione: ",vettoreScaricoTitoli(i)%tipo_operazione&
            &, "appendice: ", vettoreScaricoTitoli(i)%numero_appendice, "Differenza tra i premi netti: ", &
            &abs(premio_netto_dettagliato - vettoreScaricoTitoli(i)%premio_netto), "Premio netto titoli dettagliati: ",&
            &premio_netto_dettagliato, "Premio netto titolo", vettoreScaricoTitoli(i)%premio_netto
            print*, "***********************************************************************************************************"
            quadratura = .false.
        end if
    end do 
    if(quadratura)then
        print*,"Non sono presenti squadrature."
    end if
    write(*,'(a14)', advance='no')  "Premi invio. "
    read(*, '(A)') input
    
end subroutine

!questa routine verifica la quadratura delle tasse totali tra lo scarico dettagliato e e lo scarico aggregato
subroutine verifica_quadratura_tasse(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_titoli), pointer :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file
    type(scarico_dettagliato_titoli), pointer :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    
    integer :: i,j
    real(kind = maxPrecisione) :: tasse_totali_bdx, tasse_1_rata_dettagliato, differenza
    character(len=1) :: inputCarattere
    logical :: quadratura 
    
    quadratura = .true.
    print*, "Sono nella routine che verifica la quadratura delle tasse tra i due scarichi"
    
    !ciclo sui titoli dello scarico decadale
    do i = 1, dimensioneVettoreScaricoTitoli

        tasse_totali_bdx = vettoreScaricoTitoli(i)%imposte_totali + vettoreScaricoTitoli(i)%ssn + &
          & vettoreScaricoTitoli(i)%antiraket_tasse + vettoreScaricoTitoli(i)%prov_tasse
          
        !ciclo sullo scarico titoli dettaglati per garanzia, e sommo le garanzie per verificare la quadratura 
        tasse_1_rata_dettagliato = 0  
        do J=1, dimensioneVettoreScaricoDettagliatoTitoli
            if((vettoreScaricoTitoli(i)%numero_polizza == vettoreScaricoDettagliatoTitoli(j)%numero_polizza) .and. &
                &(vettoreScaricoTitoli(i)%tipo_operazione == vettoreScaricoDettagliatoTitoli(j)%operation_type).and. &
                &(vettoreScaricoTitoli(i)%numero_appendice == vettoreScaricoDettagliatoTitoli(j)%numero_appendice)) then
                tasse_1_rata_dettagliato = tasse_1_rata_dettagliato + vettoreScaricoDettagliatoTitoli(j)%pdgar_tasse1rata
            end if
        end do
        
        differenza = ABS(tasse_1_rata_dettagliato - tasse_totali_bdx)
        if (differenza > 0.01) then
            quadratura = .false.
            print*, "______________________________________________________________________________________________________________"
            print*, "Squadratura: ", differenza, "numero polizza: ", vettoreScaricoTitoli(i)%numero_polizza, &
                & "tasse totali bourderaux: ", tasse_totali_bdx, "tasse totali dettaglato: ", tasse_1_rata_dettagliato
        end if
    end do
    
    if(quadratura)then
        print*,"Non sono presenti squadrature."
    end if
    print*, "Ho terminato di effettuare la quadratura"
    write(*,'(a14)', advance='no')  "Premi invio. "
    read(*, '(A)') inputCarattere

end subroutine


!Questa procedura verifica la quadratura tra i due scarichi
subroutine verifica_quadratura_scarichi(vettoreScaricoTitoli, dimensioneVettoreScaricoTitoli, vettoreScaricoDettagliatoTitoli,&
                                        &dimensioneVettoreScaricoDettagliatoTitoli)
    implicit none
    
    !puntatore al vettore contenente i titoli letti dal file decadale
    type(scarico_titoli), POINTER :: vettoreScaricoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore
    integer :: dimensioneVettoreScaricoTitoli
    !puntatore al vettore contenente i titoli letti dal file titoli dettagliato per garanzia
    type(scarico_dettagliato_titoli), POINTER :: vettoreScaricoDettagliatoTitoli(:)
    !numero di titoli presenti nel file e dimensione del vettore contemente i titoli dettagliati per garanzia
    integer :: dimensioneVettoreScaricoDettagliatoTitoli 
    
    integer :: i,j
    character(len=1) :: inputCarattere
    real(kind = maxPrecisione) :: totale_netto1rata, totale_tasse1rata, totale_abbuono1rata, totale_contributo_ssn,&
        & totale_antiraket, totale_imposte_provinciali, premio_netto_dettagliato 
    real(kind = maxPrecisione) :: totale_premio_netto_titoli, differenza_premi_netti, totale_premio_lordo_titoli, &
        & totale_tasse_titoli, imposte_totali_titoli, totale_ssn_titoli, totale_antiraket_titoli, &
        & totale_prov_tasse_titoli, totale_imposte_titoli
        
    real(kind = maxPrecisione) :: differenza_totale_tasse, differenza_totale_tasse_titoli, differenza_ssn, differenza_antiraket,&
        & differenza_tasse_provinciali
    
    totale_netto1rata = 0
    totale_tasse1rata = 0
    totale_abbuono1rata = 0
    totale_contributo_ssn = 0
    totale_antiraket = 0 
    totale_imposte_provinciali = 0
    premio_netto_dettagliato = 0
    
    totale_premio_netto_titoli = 0
    differenza_premi_netti = 0
    totale_premio_lordo_titoli = 0
    totale_tasse_titoli = 0
    imposte_totali_titoli = 0
    totale_ssn_titoli = 0
    totale_antiraket_titoli = 0
    totale_prov_tasse_titoli = 0
    totale_imposte_titoli = 0
    
    print*,""
    print*,""
    print*, "Questa routine verifica la quadratura tra i due scarichi."
    !ciclo sullo scarico dettagliato e calcolo le somme totali del premio netto,tasse1rata, abbuono1rata,
    !ssn, antiraket, imposte provinciali---punto uno delle regole di quadratura
    do J=1, dimensioneVettoreScaricoDettagliatoTitoli        
        totale_netto1rata = totale_netto1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_netto1rata
        totale_tasse1rata = totale_tasse1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_tasse1rata
        totale_abbuono1rata = totale_abbuono1rata + vettoreScaricoDettagliatoTitoli(j)%pdgar_abbuono1rata
        totale_contributo_ssn = totale_contributo_ssn + vettoreScaricoDettagliatoTitoli(j)%contributo_ssn 
        totale_antiraket = totale_antiraket + vettoreScaricoDettagliatoTitoli(j)%antiraket
        totale_imposte_provinciali = totale_imposte_provinciali + vettoreScaricoDettagliatoTitoli(j)%imposte_provinciali     
    end do
    
    
    !ciclo sullo scarico titoli e calcolo i valori totali di: premio netto, premio lordo, imposte totali, ssn, antiraket e 
    !tasse provinciali
    do j = 1, dimensioneVettoreScaricoTitoli
        totale_premio_netto_titoli = totale_premio_netto_titoli + vettoreScaricoTitoli(j)%premio_netto
        totale_premio_lordo_titoli = totale_premio_lordo_titoli + vettoreScaricoTitoli(j)%premio_lordo
        imposte_totali_titoli = imposte_totali_titoli + vettoreScaricoTitoli(j)%imposte_totali
        totale_ssn_titoli = totale_ssn_titoli + vettoreScaricoTitoli(j)%ssn
        totale_antiraket_titoli = totale_antiraket_titoli + vettoreScaricoTitoli(j)%antiraket_tasse
        totale_prov_tasse_titoli = totale_prov_tasse_titoli + vettoreScaricoTitoli(j)%prov_tasse
    end do
    
    !Stampo i valori precedentemente calcolati
    print*, ""
    print*, "Valori aggregati dello scarico titoli"
    print*, "Premio netto titoli: ", totale_premio_netto_titoli
    print*, "Premio lordo titoli: ", totale_premio_lordo_titoli
    print*, "Totale imposte titoli: ", imposte_totali_titoli
    print*, "Totale ssn titoli: ", totale_ssn_titoli
    print*, "Totale antiraket titoli: ", totale_antiraket_titoli
    print*, "Totale tasse provinciali titoli: ", totale_prov_tasse_titoli
    
    print*, ""
    print*, "Valori aggregati dello scarico dettagliato titoli"
    print*, "Premi netti dettagliato: ", totale_netto1rata
    print*, "Abbuono dettagliato: ", totale_abbuono1rata
    print*, "Totale tasse prima rata: ", totale_tasse1rata
    print*, "Totale contributo ssn: ", totale_contributo_ssn
    print*, "Totale antiraket: ", totale_antiraket
    print*, "Totale imposte provinciali: ", totale_imposte_provinciali

    ! punto due delle regole di quadratura --- dal premio netto prima rata sotrarre l'abbuono prima rata
    premio_netto_dettagliato = totale_netto1rata - totale_abbuono1rata
    !print*, "Premi netti effettivi dettagliato(netto-abbuono): ", premio_netto_dettagliato
    
    !punto due delle regole di quadratura--- i premi netti nei due scarichi devono coincidere
    !calcolo la differenza tra i premi netti dei due scarichi
    differenza_premi_netti = ABS(premio_netto_dettagliato - totale_premio_netto_titoli)
    print*, ""
    print*, "Premi netti titoli(sommando la colonna premio netto dello scarico aggregato): ", totale_premio_netto_titoli
    print*, "Premi netti titoli dettagliati(netto-abbuono dello scarico dettagliato): ", premio_netto_dettagliato
    if (differenza_premi_netti < 0.01)then
        print*, "OK quadratura premi netti: ", differenza_premi_netti
    else 
        print*, "SQUADRATURA premi netti: ", differenza_premi_netti
    end if
    

    
    !punto tre delle regole di quadratura--- nello scarico dei titoli sotrarre dal premio lordo il premio netto ottenendo
    !il totale delle tasse; il valore deve coincidere nei due scarichi
    totale_tasse_titoli = totale_premio_lordo_titoli - totale_premio_netto_titoli
    print*, ""
    print*, "Totale tasse titoli(calcolate per differenza tra il premio lordo e il premio netto): ", totale_tasse_titoli
    print*, "Totale tasse titoli dettagliati(calcolato sommando la tassa prima rata dello scarico dettagliato): ", totale_tasse1rata
    differenza_totale_tasse = ABS(totale_tasse_titoli - totale_tasse1rata)
    if (differenza_totale_tasse < 0.01)then
        print*, "OK quadratura totale tasse: ", differenza_totale_tasse
    else 
        print*, "SQUADRATURA totale tasse: ", differenza_totale_tasse
    end if
    
 
    !Punto quattro delle regole di quadratura--- nello scarico titoli sommare: inposta base + ssn + antiracket + provinciali, devo
    !ottenere il totale delle tasse( lordo - netto)
    print*, ""
    totale_imposte_titoli = imposte_totali_titoli + totale_ssn_titoli + totale_antiraket_titoli + totale_prov_tasse_titoli
    print*, "Totale imposte titoli aggregati calcolata sommando(Imp base+ssn+antiracket+provinciali): ", totale_imposte_titoli    
    differenza_totale_tasse_titoli=ABS(totale_tasse_titoli - totale_imposte_titoli )
    if (differenza_totale_tasse_titoli < 0.01)then
        print*, "OK quadratura totale tasse titoli: ", differenza_totale_tasse_titoli
    else 
        print*, "SQUADRATURA totale tasse titoli: ", differenza_totale_tasse_titoli
    end if
    
    !punto cinque delle regole di quadratura: il valore totale del contibuto ssn nei due scarichi deve coincidere
    print*, ""
    print*, "Totale ssn titoli: ", totale_ssn_titoli
    print*, "Totale ssn titoli dettagliati: ", totale_contributo_ssn
    differenza_ssn = ABS(totale_ssn_titoli - totale_contributo_ssn)
    if (differenza_ssn < 0.01)then
        print*, "OK quadratura ssn: ", differenza_ssn
    else 
        print*, "SQUADRATURA ssn: ", differenza_ssn
    end if
    
    !punto cinque delle regole di quadratura: il valore totale del contibuto antiraket nei due scarichi deve coincidere
    print*, ""
    print*, "Totale antiraket titoli: ", totale_antiraket_titoli
    print*, "Totale antiraket titoli dettagliati: ", totale_antiraket
    differenza_antiraket = ABS(totale_antiraket_titoli - totale_antiraket)
    if (differenza_antiraket < 0.01)then
        print*, "OK quadratura antiraket: ", differenza_antiraket
    else 
        print*, "SQUADRATURA antiraket: ", differenza_antiraket
    end if
    
    !punto cinque delle regole di quadratura: il valore totale delle tasse provinciali nei due scarichi deve coincidere
    print*, ""
    print*, "Totale imposte provinciali titoli: ", totale_prov_tasse_titoli
    print*, "Totale imposte provinciali titoli dettagliati: ", totale_imposte_provinciali
    differenza_tasse_provinciali = ABS(totale_prov_tasse_titoli - totale_imposte_provinciali)
    if (differenza_tasse_provinciali < 0.01)then
        print*, "OK quadratura tasse provinciali: ", differenza_tasse_provinciali
    else 
        print*, "SQUADRATURA tasse provinciali: ", differenza_tasse_provinciali
    end if
    
    write(*,'(a14)', advance='no')  "Premi invio. "
    read(*, '(A)') inputCarattere
end subroutine 

END MODULE controllo_quadratura_scarichi
