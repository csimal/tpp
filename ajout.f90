!Fonctions similaires à read_constraints_gen
!pour les autres contraintes
!à rajouter dans le fichier main si validées.

!Modifications à read_constraints_gen:
! 1) Rajouter un entier pour l'indice du vecteur
! 2) changer le nom de la subroute , il manque un mot
! 3) Penser à fermer le fichier...
subroutine read_constraints_gen(ins,const_gen)
    integer, dimension(20), intent(inout) :: const_gen
    integer, intent(in) :: ins
    !rajouter un entier i pour la position du vecteur const_gen
	integer :: iostatus,i
    character(len=5) :: gen
    integer :: com, val
    open (unit = 1, file = "ContrainteAge.txt", action="read")
    read(1,*)
    do while (1.eq.1)
        read(1,*,IOSTAT=iostatus) com, gen, val
        if (iostatus<0) then
            exit
        end if
        if (com==ins) then
            i=str2gen(gen)
			const_gen(i)=val
        end if
    end do
	!nom de la subroutine à changer
	!fermer le fichier
	close(1)
end subroutine read_constraints_gen



! Contraintes pour le sexe

function str2sex(sex) result(i)
    character, dimension(6), intent(in) :: sex
    integer :: i
    select case (sex)
            case ("Femmes")
                i=1
            case ("Hommes")
                i=2
    end select
end function str2sex

!Contraintes pour le diplome

function str2dipl(dipl) result(i)
    character, dimension(11), intent(in) :: dipl
    integer :: i
    select case (dipl)
            case ("Aucun")
                i=1
            case ("CITE1")
                i=2
            case ("CITE2")
                i=3
            case ("CITE3")
                i=4
            case ("CITE4")
                i=5
            case ("CITE5")
                i=6
            case("CITE6")
                i=7
            case("NonConcerne")
                i=8
    end select
end function str2dipl

!Contraintes pour le statut

function str2stat(stat) result(i)
    character, dimension(12), intent(in) :: stat
    integer :: i
    select case (stat)
            select case (stat)
            case ("Chomeurs")
                i=1
            case ("Inactifs")
                i=2
            case ("Travailleurs")
                i=3
    end select
end function str2stat
