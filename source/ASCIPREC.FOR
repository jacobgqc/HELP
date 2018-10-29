PROGRAM SCANR
  !
  !
  !********************************************************************
  !    THIS ROUTINE CONVERTS AN ASCII FILE TO A FORMATTED FILE
  !    CONTAINING DAILY VALUES.  EACH LINE OF OUTPUT CONSISTS OF
  !    THE YEAR,  TEN DATA VALUES, AND THE LINE NUMBER.
  !********************************************************************

        INTEGER*2 iflag , nl , nh
        CHARACTER txt*80 , infile*60 , city*20 , state*20
        CHARACTER city2*20 , state2*20 , outfile*12
        LOGICAL lval
        REAL indata(370) , rvalue
        DATA txt/                                                         &
       &'                                                                 &
       &               '/

       !
       !  INITIALIZE VARIBLES
       !
             infile = txt(1:60)
             city = txt(1:20)
             state = txt(1:20)
             city2 = txt(1:20)
             state2 = txt(1:20)
             lval = .FALSE.
             iconvert = 0
       !
       ! READ INPUT PARAMETERS - CITY, STATE, AND DATA UNITS.
       !
      INQUIRE(FILE='SCAN.TMP', EXIST=LVAL)
      IF (.NOT. LVAL) THEN
         CALL UTLTY (256,1,1,NL,NH,1,' ')
         CALL UTLTY (8+16,2,16,NL,NH,43,'<<< SCAN.TMP INPUT FILE DOES NO
     *T EXIST! >>>')
         CALL UTLTY (8, 6, 1, NL, NH, 1, ' ')
         WRITE (6, 41)
41       FORMAT(
     *15x,'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»',/,
     *15x,'º                                              º',/,
     *15x,'º         Enter the following information:     º',/,
     *15x,'º                                              º',/,
     *15x,'º        CITY  :                               º',/,
     *15x,'º        STATE :                               º',/,
     *15x,'º        UNITS (1=U.S.  2=METRIC) :            º',/,
     *15x,'º                                              º',/,
     *15x,'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼')
         CALL ACELL (11, 33, 20, CITY, NL, NH, 1)
         CALL ACELL (12, 33, 20, STATE, NL, NH, 1)
50       CALL CELL (13,52,4, NL, NH, RVALUE, IFLAG, TXT)
         IF (RVALUE .NE. 1.0 .AND. RVALUE .NE. 2.0) GOTO 50
         IUNIT = RVALUE
      ELSE
         OPEN (20, FILE = 'SCAN.TMP', STATUS = 'OLD')
         READ (20,100, END = 120) CITY
         READ (20,100, END = 120) STATE
100      FORMAT(A20)
         READ (20,105, END = 120) IUNIT
105      FORMAT(I1)
120      CONTINUE
         CLOSE (20)
      ENDIF
      CALL TOUPPER(CITY,20)
      CALL TOUPPER(STATE,20)
      IUNIT2 = IUNIT

C
C       SEE IF THEY WANT TO ADD DATA TO EXISTING FILE
C
      OUTFILE(1:9) = 'DATA4.TMP'
      INQUIRE(FILE=OUTFILE, EXIST=LVAL)
      IF (LVAL) THEN
         CALL APPEND (IFLAG)
         IF (IFLAG .EQ. 1) THEN
            OPEN (10, FILE=OUTFILE, STATUS='OLD')
            READ(10,106) ITMP
            READ(10,106) IUNIT2
 106        FORMAT(I2)
            READ(10,135) CITY2, STATE2
135         FORMAT(A20,A20)
            CALL TOUPPER(CITY2,20)
            CALL TOUPPER(STATE2,20)
            IF(CITY .NE. CITY2 .OR. STATE .NE. STATE2) THEN
               CALL UTLTY (8+256,10,10,NL,NH,1,' ')
               WRITE(6,145) CITY2, STATE2, CITY, STATE
145            FORMAT(5x,'CITY AND STATE IN MASTER FILE  < ',A20,A20,'>'
     *,/,5x,'******* DOES NOT MATCH *******',/,5x,'CITY AND STATE IN INP
     *UT FILE   < ',A20,A20,'>')
               CALL UTLTY (8+18+128,18,25,NL,NH,30,'PRESS ANY KEY TO EXI
     *T PROGRAM!')
               GOTO 999
            ENDIF
            IF (IUNIT .NE. IUNIT2) THEN
               ICONVERT = IUNIT2
               CALL UTLTY (256,1,1,NL,NH,1,' ')
               CALL UTLTY (8+16,10,12,NL,NH,56,' DATA WILL BE CONVERTED
     *TO UNITS PRESENT IN MASTER FILE.')
               IF(ICONVERT .EQ. 1) THEN
                  CALL UTLTY (8+16,11,27,NL,NH,27,'(FROM METRIC TO U.S.
     *UNITS)')
               ELSE
                  CALL UTLTY (8+16,11,27,NL,NH,27,'(FROM U.S. TO METRIC
     *UNITS)')
               ENDIF
               CALL UTLTY (8+16+128,14,27,NL,NH,26,'PRESS ANY KEY TO CON
     *TINUE!')
               GOTO 180
            ENDIF
            GOTO 180
         ENDIF
      ENDIF

C
C  CREATE NEW MASTER FILE
C

      CALL ERASE(OUTFILE(1:12),*151)
151   OPEN (10, FILE=OUTFILE, STATUS='NEW')
      WRITE(10,161) 7,IUNIT,CITY,STATE,'  '
161   FORMAT(I1/I1/A20,A20/A2)
      GOTO 200

180   CLOSE(10)
      OPEN (10, FILE=OUTFILE, STATUS='OLD', ACCESS='APPEND')

C
C       WRITE FIRST SCREEN TO INPUT FILE NAME THE USER WISHES TO USE AS
C       INPUT - FILE IS ASSUMED TO BE ASCII FILE AND WILL BE OUTPUT AS
C       FORMATTED FILE CALLED "DATA?.TMP". ? = (4=rain 7=temp 13=srad)
C

200   CALL UTLTY (8+256, 6, 1, NL, NH, 1, ' ')
      WRITE (6, 241) CHAR(39)
241   FORMAT(
     *15x,'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»',/,
     *15x,'º                                              º',/,
     *15x,'º    Enter the name of the data set for the    º',/,
     *15x,'º             current year',A1,'s data.             º',/,
     *15x,'º                                              º',/,
     *15x,'º       Rainfall input values should           º')
      IF (IUNIT .EQ. 1) WRITE (6, 261)
261      FORMAT(
     *15x,'º               be in inches.                  º')
      IF (IUNIT .EQ. 2) WRITE (6, 271)
271      FORMAT(
     *15x,'º          be in millimeters (mm).             º')
      WRITE (*, 281)
281   FORMAT(
     *15x,'º                                              º',/,
     *15x,'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼',/,
     *15x,'          Enter ESC to exit this session. ')

      CALL ACELL (14, 18, 40, INFILE, NL, NH, 1)
C
C   CHECK FOR ESC TO EXIT
C
      IF (NL .EQ. 27) GO TO 999
      INQUIRE (FILE=INFILE, EXIST=LVAL)
      IF(.NOT. LVAL) THEN
         CALL UTLTY (256,1,1,NL,NH,1,' ')
         CALL UTLTY (8+16,10,20,NL,NH,39,'<<< INPUT DATA FILE DOES NOT E
     *XIST! >>>')
         CALL UTLTY (8+16+128,15,27,NL,NH,26,'PRESS ANY KEY TO CONTINUE!
     *')
         GO TO 200
      ENDIF

300   CALL UTLTY (8+256, 7, 1, NL, NH, 1, ' ')
      WRITE (6, 341)
341   FORMAT(
     *15x,'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»',/,
     *15x,'º                                              º',/,
     *15x,'º     Enter the year of the rainfall data      º',/,
     *15x,'º     contained in this file being added.      º',/,
     *15x,'º                                              º',/,
     *15x,'º                                              º',/,
     *15x,'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼')

      CALL CELL (13,18,4, NL, NH, RVALUE, IFLAG, TXT)
      IF(RVALUE .EQ. 0) GOTO 300
      IYEAR = (RVALUE + 0.001)

C
C     Check for leap year.
C

      IF (((IYEAR/4)*4) .NE. IYEAR) THEN
         INUMDAYS = 365
      ELSE
         INUMDAYS = 366
      ENDIF

C
C     INITIALIZE ARRAY AND READ INPUT FILE
C

      DO 361 I=1, INUMDAYS
         INDATA(I) = 9999.0
361   CONTINUE
      DO 366 I=INUMDAYS+1, 370
         INDATA(I) = 0.0
366   CONTINUE
      OPEN(11, FILE=INFILE, STATUS='OLD')
      READ(11,*, END = 410) (INDATA(I), I=1, INUMDAYS+1)
      CLOSE(11)

C
C  TO MUCH DATA IN INPUT FILE
C

      CALL KEEPER(IFLAG,2)
      IF(IFLAG .EQ. 0) GOTO 200
      INDATA(INUMDAYS+1) = 0.0
      GOTO 500

C
C  CHECK IF NOT ENOUGH DATA
C

410   CLOSE(11)
      DO 411 I=1, INUMDAYS
        IF(INDATA(I) .EQ. 9999) GOTO 420
411   CONTINUE
      GOTO 500

C
C  NOT ENOUGH DATA IN INPUT FILE
C

420   CALL UTLTY (256,1,1,NL,NH,1,' ')
      CALL UTLTY (8+16,8,23,NL,NH,36,'NOT ENOUGH DATA FOUND IN INPUT FIL
     *E!')
      WRITE(6, 446) I-1, INUMDAYS
446   FORMAT(23x,'-------------------------------------',/,23x,'TOTAL NU
     *MBER OF VALUES READ  ---> ',I4,/,23x,'TOTAL NUMBER OF DAYS IN YEAR
     * ---> ',I4)
      CALL UTLTY (8+16+128,16,27,NL,NH,26,'PRESS ANY KEY TO CONTINUE!')
      GOTO 200

C
C  CHECK IF DATA NEEDS UNITS CONVERTED
C

500   IF(ICONVERT .EQ. 1) THEN
         DO 531 I=1, INUMDAYS
            INDATA(I) = INDATA(I) / 25.4
531      CONTINUE
      ELSE IF(ICONVERT .EQ. 2) THEN
         DO 541 I=1, INUMDAYS
            INDATA(I) = INDATA(I) * 25.4
541      CONTINUE
      ENDIF

C
C  WRITE DATA TO MASTER FILE FORMATTED
C

      IF (IUNIT2 .EQ. 1) THEN
         DO 551 I=1, INUMDAYS, 10
            WRITE(10,546) IYEAR,(INDATA(J),J=I,I+9),(J/10)
546         FORMAT(I10,10F5.2,I10)
551      CONTINUE
      ELSE
         DO 571 I=1, INUMDAYS, 10
            WRITE(10,566) IYEAR,(INDATA(J),J=I,I+9),(J/10)
566         FORMAT(I10,10F5.1,I10)
571      CONTINUE
      ENDIF
      GOTO 200
999   STOP
END
