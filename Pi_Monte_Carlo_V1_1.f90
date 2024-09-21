!#############################################################################
!
!   PROGRAMME : Pi_Monte_Carlo.f90
!
!   Version : 1.1
!
!   OBJET : Approxime la valeur de π
!
!   DATE/AUTEUR/MODIFICATIONS
!
!           21/09/2024 (F. HENRY) : Création          
!
!
!#############################################################################

program Pi_Monte_Carlo

    implicit none
    
    integer :: nb_p_i                               !Nombre de points utilisés pour approximer pi
    integer :: i,j_i=0                              !Variables utilisées dans des boucles do
    real :: x,y,r                                   !Variables utilisées pour définir les coordonées des points et leurs distances par rapport à l'origine
    logical :: vld_i=.false.                        !Variable vérifiant la validité de valeurs rentrées par l'utilisateur

!
!   Instruction à l'utilisateur
!

    print *, 'Bonjour, determinons une approximation de pi grace a la methode de Monte Carlo.'

!
!   Boucle d'implémentation du nombre de points
!

    do while (.not. vld_i)

        print *, 'Combien de points souhaitez-vous utiliser pour approximer pi ? :'                 !Invitation à l'utilisateur d'entrer une valeur entière  
        read *, nb_p_i                                                                              !Lecture au clavier

        ! Vérification si l'entrée est un entier naturel
        if (nb_p_i > 0) then

            vld_i = .true.                                                                          ! L'entrée est valide, sort de la boucle

        else

            print *, 'Il semble que votre nombre n''est pas un entier naturel. Veuillez reessayer.'

        end if

    end do

!
!   Boucle d'approximation de pi
!

    do i=1,nb_p_i
    
        call random_number(x)
        call random_number(y)
        r = sqrt(x**2+y**2)

        if (r<1) then

            j_i=j_i+1

        end if

    end do

    print *, 'L''approximation de pi est :', 4.0 * real(j_i) / real(nb_p_i)

!
!   Section supplémentaire pour utiliser PiMonteCarlo.exe sans fermeture immédiate
!

    print *, 'Appuyez sur ENTREE pour terminer...'      !Invite l'utilisateur à fermer le programme 
    read(*,*)                                           !Attend que l'utilisateur appuie sur une touche

end program Pi_Monte_Carlo