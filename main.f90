
program SpaceInvaders

    use iso_c_binding
    implicit none

    ! ==========================================
    ! WINDOWS API INTERFACE
    ! ==========================================

    interface

        function GetStdHandle(n) &
            bind(C, name="GetStdHandle") result(h)
            import
            integer(c_int32_t), value :: n
            type(c_ptr) :: h
        end function

        function GetConsoleMode(h, m) &
            bind(C, name="GetConsoleMode") result(r)
            import
            type(c_ptr), value :: h
            integer(c_int32_t) :: m
            integer(c_int) :: r
        end function

        function SetConsoleMode(h, m) &
            bind(C, name="SetConsoleMode") result(r)
            import
            type(c_ptr), value :: h
            integer(c_int32_t), value :: m
            integer(c_int) :: r
        end function

        function kbhit() bind(C, name="_kbhit") result(r)
            import
            integer(c_int) :: r
        end function

        function getch() bind(C, name="_getch") result(r)
            import
            integer(c_int) :: r
        end function

        subroutine Sleep(ms) bind(C, name="Sleep")
            import
            integer(c_int32_t), value :: ms
        end subroutine

    end interface

    ! ==========================================
    ! GAME CONSTANTS
    ! ==========================================

    integer, parameter :: W = 70
    integer, parameter :: H = 22

    integer, parameter :: NC = 8
    integer, parameter :: NR = 4
    integer, parameter :: NB = 4

    integer, parameter :: PLAYER_Y = 20

    ! ==========================================
    ! GAME VARIABLES
    ! ==========================================

    integer :: ax(NC,NR)
    integer :: ay(NC,NR)

    logical :: alive(NC,NR)

    integer :: bx(NB)
    integer :: by(NB)

    logical :: bullet_active(NB)

    integer :: player_x
    integer :: score
    integer :: lives

    integer :: direction
    integer :: frame
    integer :: cooldown

    integer :: remaining
    integer :: i, j, k
    integer :: key

    logical :: game_over
    logical :: restart
    logical :: breach

    character :: screen(H,W)
    character(len=W+2) :: line

    type(c_ptr) :: hout, hin

    integer(c_int32_t) :: old_out
    integer(c_int32_t) :: old_in

    integer(c_int) :: result

    logical :: output_saved
    logical :: input_saved

    ! ==========================================
    ! INITIALIZE WINDOWS CONSOLE
    ! ==========================================

    hout = GetStdHandle(-11_c_int32_t)
    hin  = GetStdHandle(-10_c_int32_t)

    output_saved = .false.
    input_saved = .false.

    result = GetConsoleMode(hout, old_out)

    if (result /= 0) then

        output_saved = .true.

        result = SetConsoleMode(hout, &
            ior(old_out, 4_c_int32_t))

    end if

    result = GetConsoleMode(hin, old_in)

    if (result /= 0) then

        input_saved = .true.

        result = SetConsoleMode(hin, &
            iand(old_in, not(6_c_int32_t)))

    end if

    ! ==========================================
    ! START GAME
    ! ==========================================

    restart = .true.

    do while (restart)

        ! --------------------------------------
        ! INITIALIZE PLAYER
        ! --------------------------------------

        player_x = W / 2

        score = 0
        lives = 3

        direction = 1
        frame = 0
        cooldown = 0

        game_over = .false.

        ! --------------------------------------
        ! INITIALIZE ALIENS
        ! --------------------------------------

        do i = 1, NC

            do j = 1, NR

                ax(i,j) = 7 + (i-1)*7
                ay(i,j) = 3 + (j-1)*2

                alive(i,j) = .true.

            end do

        end do

        ! --------------------------------------
        ! INITIALIZE BULLETS
        ! --------------------------------------

        bx = 0
        by = 0

        bullet_active = .false.

        ! ======================================
        ! MAIN GAME LOOP
        ! ======================================

        do while (.not. game_over)

            frame = frame + 1

            ! ----------------------------------
            ! 1. KEYBOARD INPUT
            ! ----------------------------------

            do while (kbhit() /= 0)

                key = getch()

                select case (key)

                case (iachar('a'), iachar('A'))

                    player_x = max(2, player_x-2)

                case (iachar('d'), iachar('D'))

                    player_x = min(W-1, player_x+2)

                case (32)

                    ! FIRE BULLET

                    if (cooldown == 0) then

                        do k = 1, NB

                            if (.not. bullet_active(k)) then

                                bx(k) = player_x
                                by(k) = PLAYER_Y-1

                                bullet_active(k) = .true.

                                cooldown = 5

                                exit

                            end if

                        end do

                    end if

                case (iachar('q'), iachar('Q'))

                    game_over = .true.

                end select

            end do

            if (game_over) exit

            if (cooldown > 0) then
                cooldown = cooldown - 1
            end if

            ! ----------------------------------
            ! 2. MOVE ALIENS
            ! ----------------------------------

            if (mod(frame,8) == 0) then

                breach = .false.

                do i = 1, NC

                    do j = 1, NR

                        if (alive(i,j)) then

                            if (ax(i,j)+direction < 2 .or. &
                                ax(i,j)+direction > W-1) then

                                breach = .true.

                            end if

                        end if

                    end do

                end do

                if (breach) then

                    direction = -direction

                    do i = 1, NC

                        do j = 1, NR

                            if (alive(i,j)) then

                                ay(i,j) = ay(i,j)+1

                            end if

                        end do

                    end do

                else

                    do i = 1, NC

                        do j = 1, NR

                            if (alive(i,j)) then

                                ax(i,j) = ax(i,j)+direction

                            end if

                        end do

                    end do

                end if

            end if

            ! ----------------------------------
            ! 3. MOVE BULLETS
            ! ----------------------------------

            do k = 1, NB

                if (bullet_active(k)) then

                    by(k) = by(k)-1

                    if (by(k) <= 1) then

                        bullet_active(k) = .false.

                    end if

                end if

            end do

            ! ----------------------------------
            ! 4. COLLISION DETECTION
            ! ----------------------------------

            do k = 1, NB

                if (.not. bullet_active(k)) cycle

                do i = 1, NC

                    do j = 1, NR

                        if (alive(i,j)) then

                            if (by(k) == ay(i,j) .and. &
                                abs(bx(k)-ax(i,j)) <= 1) then

                                alive(i,j) = .false.

                                bullet_active(k) = .false.

                                score = score + 10

                                exit

                            end if

                        end if

                    end do

                    if (.not. bullet_active(k)) exit

                end do

            end do

            ! ----------------------------------
            ! 5. COUNT REMAINING ALIENS
            ! ----------------------------------

            remaining = count(alive)

            if (remaining == 0) then

                game_over = .true.

            end if

            ! ----------------------------------
            ! 6. CHECK ALIEN BREACH
            ! ----------------------------------

            breach = .false.

            do i = 1, NC

                do j = 1, NR

                    if (alive(i,j)) then

                        if (ay(i,j) >= PLAYER_Y-1) then

                            breach = .true.

                        end if

                    end if

                end do

            end do

            if (breach) then

                lives = lives-1

                ! Reset surviving aliens

                do i = 1, NC

                    do j = 1, NR

                        if (alive(i,j)) then

                            ay(i,j) = 3+(j-1)*2

                        end if

                    end do

                end do

                if (lives <= 0) then

                    game_over = .true.

                end if

            end if

            if (game_over) exit

            ! ----------------------------------
            ! 7. DRAW GAME SCREEN
            ! ----------------------------------

            screen = ' '

            ! Draw aliens

            do i = 1, NC

                do j = 1, NR

                    if (alive(i,j)) then

                        screen(ay(i,j),ax(i,j)) = 'W'

                    end if

                end do

            end do

            ! Draw bullets

            do k = 1, NB

                if (bullet_active(k)) then

                    if (by(k) >= 1 .and. by(k) <= H) then

                        screen(by(k),bx(k)) = '|'

                    end if

                end if

            end do

            ! Draw spaceship

            screen(PLAYER_Y,player_x) = '^'

            ! ----------------------------------
            ! 8. DISPLAY GAME
            ! ----------------------------------

            write(*,'(A)',advance='no') &
                achar(27)//'[H'

            write(*,'(A)') &
                achar(27)//'[1;36m'// &
                '          SPACE INVADERS'// &
                achar(27)//'[0m'

            write(*,'(A,I5,A,I3,A,I3)') &
                ' SCORE: ', score, &
                '   LIVES: ', lives, &
                '   ALIENS: ', remaining

            write(*,'(A)') &
                '+'//repeat('-',W)//'+'

            do i = 1, H

                line = ' '

                line(1:1) = '|'

                do j = 1, W

                    line(j+1:j+1) = screen(i,j)

                end do

                line(W+2:W+2) = '|'

                write(*,'(A)') line

            end do

            write(*,'(A)') &
                '+'//repeat('-',W)//'+'

            write(*,'(A)') &
                ' A: LEFT   D: RIGHT   SPACE: SHOOT   Q: QUIT'

            ! ----------------------------------
            ! 9. GAME SPEED
            ! ----------------------------------

            call Sleep(50_c_int32_t)

        end do

        ! ======================================
        ! GAME OVER SCREEN
        ! ======================================

        write(*,'(A)',advance='no') &
            achar(27)//'[2J'//achar(27)//'[H'

        print *, "================================"
        print *, "           GAME OVER!"
        print *, "================================"

        print *, "FINAL SCORE: ", score
        print *, " "

        if (remaining == 0) then

            print *, "CONGRATULATIONS! YOU WON!"

        else

            print *, "BETTER LUCK NEXT TIME!"

        end if

        print *, " "
        print *, "Press R to restart."
        print *, "Press Q to quit."

        restart = .false.

        do

            if (kbhit() /= 0) then

                key = getch()

                if (key == iachar('r') .or. &
                    key == iachar('R')) then

                    restart = .true.

                    exit

                else if (key == iachar('q') .or. &
                         key == iachar('Q')) then

                    exit

                end if

            end if

            call Sleep(50_c_int32_t)

        end do

    end do

    ! ==========================================
    ! RESTORE WINDOWS CONSOLE
    ! ==========================================

    if (output_saved) then

        result = SetConsoleMode(hout,old_out)

    end if

    if (input_saved) then

        result = SetConsoleMode(hin,old_in)

    end if

    write(*,'(A)',advance='no') &
        achar(27)//'[0m'//achar(27)//'[?25h'

    print *, "Thanks for playing!"

end program SpaceInvaders
