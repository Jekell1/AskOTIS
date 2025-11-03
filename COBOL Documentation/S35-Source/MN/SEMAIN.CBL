       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SEMAIN.
      ******************************************************************
      *  DESC:   SECURITY FILE MAINTENANCE
      *
      * REV:
      * KEC 2015.1116 REMOVED USE OF SE-DIR & SN-DIR
      * BLM 2017.0323 CHANGE ENTRY OF SE-EXP-DATE TO 8 DIGIT.  DATE OF
      *               12/31/9999 WILL SKIP SECURITY CHECK ON PURCHASABLE
      * KEC 2020.0214 CHANGED SCANKEY TO MATCH THE SE1-KEY, FORM AND
      *                PROGRAM LOGIC.
      * BAH 2022.0218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/SEMAIN.CBL S34 06/06/21-16:50:06 >".
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL SECURITY RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SEOR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED SECURITY RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SECH".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


       01  ERRCD           PIC X VALUE " ".
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  FILLER REDEFINES CHNO.
               05  FILLER  PIC X.
               05  LETTER-TO-CHANGE
                           PIC X.

       01  ELE             PIC 9(3).
       01  HOLD-SE1-KEY.
           03  HOLD-SE-FORM    PIC X(6).

       01  IO-TYPE         PIC X(3).
       01  FIRST-SCAN      PIC 9.
       01  NEW-FLAG        PIC 9.
       01  ONE             PIC S9.
       01  CHANGE-LETTER-SUB
                           PIC 99    VALUE 0.
       01  SEPROFILEW      PIC X.

       01  ORIGINAL-SE-REC   PIC X(SE-SIZE).
       01  CHANGED-SE-REC    PIC X(SE-SIZE).

      ******************************************************************
      *     SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X.
           03  F02         PIC ZZ/ZZ/ZZZZ.
           03  F03         PIC X OCCURS 26.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(8) VALUE
           "010 020 ".
           03  FILLER      PIC X(52) VALUE
           "030 03B 03C 03D 03E 03F 03G 03H 03I 03J 03K 03L 03M ".
           03  FILLER      PIC X(52) VALUE
           "03N 03O 03P 03Q 03R 03S 03T 03U 03V 03W 03X 03Y 03Z.".

      ******************************************************************
      *     ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 3.
       01  SPEC-TABLE.
      * 1. PUCHASEABLE OPTION (Y/N)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. EXPIRATION DATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      * 3. PROFILE FOR "A"
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 3 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  SEMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SEMAIN_DEF.CPY".
       COPY "LIBMN/SEMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/SEMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       A000-MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "SEMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SEMAIN-WINDOW-HANDLE.


       A010-MAIN-MODULE.
           PERFORM FORM-RESET.


           IF EXIT-PATH NOT = "GB/COMENU"
              MOVE "N" TO CHFG(1) CHFG(2).

           PERFORM REC-SE-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM B020-ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM B030-WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
             PERFORM B040-DELETE-MODULE.

           GO TO A010-MAIN-MODULE.

      ******************************************************************
       REC-SE-MODULE SECTION.
      ******************************************************************
       REC-SE-MODULE-BEGIN.                      

           MOVE 0      TO NEW-FLAG FIRST-SCAN.     
           MOVE SPACES TO SE-FORM IO-TYPE.      

       REC-SE-MODULE-PROG.
           MOVE 0      TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-EXIT, F6-SCAN, ENTER PROGRAM NAME:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A060SEFORM." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 GO TO REC-SE-MODULE-BEGIN
              ELSE
                 MOVE SE1-KEY TO HOLD-SE1-KEY
                 GO TO END-REC-SE-MODULE.

           IF ( VDUSTATUS = "1>" )
             MOVE "END" TO IO-TYPE
             GO TO END-REC-SE-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO REC-SE-MODULE-BEGIN.

           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-SE-MODULE-PROG.

           IF ( VDUBUF = SPACES )
              GO TO REC-SE-MODULE-PROG.

           MOVE VDUBUF TO SE-FORM HOLD-SE-FORM.

       READ-SNFILE.
           MOVE SE-FORM TO SN-FORM.
           PERFORM OPEN-SN1-FILE.
           PERFORM READ-SN1-FILE.
           PERFORM CLOSE-SN1-FILE.
           IF IO-BAD
              MOVE "INVALID TRANSACTION" TO MESS
              PERFORM SEND-MESS
              GO TO REC-SE-MODULE-BEGIN
           ELSE
              MOVE "S  A300DESC." TO VDU
              MOVE SN-DESC TO VDUBUF
              PERFORM SCREENIO.

       READ-SEFILE.
           PERFORM GET-SE.

           IF ( IO-FG = 0 )
              MOVE SE-REC TO ORIGINAL-SE-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-SE-MODULE.
           EXIT.


      ******************************************************************
      *    B 0 2 0 - E N T R Y - M O D U L E
      *
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      ******************************************************************
       B020-ENTRY-MODULE SECTION.
           MOVE SPACES  TO IO-TYPE.
           MOVE 1       TO ONE.

       B020-CREATE-ROUTINE.
           IF ( NEW-FLAG NOT = 1 )
              GO TO B020-MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO B020-ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       B020-MODIFY-ROUTINE.
           MOVE 0 TO CHANGE-LETTER-SUB.

           IF EXIT-PATH = "GB/COMENU"
              MOVE
              "F1,DELETE,FIELD #,LETTER TO CHANGE,F3=ALL Y,F5=ALL N:"
                 TO MESS
           ELSE
              MOVE
              "F1, FIELD #, LETTER TO CHANGE, F3=ALL 'Y', F5=ALL 'N':"
                 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

      * F3 SETS ALL PROFILES TO "Y":
           IF VDUSTATUS = "16"
              PERFORM VARYING SESUB FROM 1 BY 1 UNTIL SESUB > 26
                 MOVE "Y" TO SE-PROF(SESUB)
              END-PERFORM
              PERFORM DISPLAY-FIELDS
              GO TO B020-MODIFY-ROUTINE.

      * F5 SETS ALL PROFILES TO "N" (EXCEPT A, ALWAYS "Y"):
           IF VDUSTATUS = "1:"
              PERFORM VARYING SESUB FROM 2 BY 1 UNTIL SESUB > 26
                 MOVE "N" TO SE-PROF(SESUB)
              END-PERFORM
              PERFORM DISPLAY-FIELDS
              GO TO B020-MODIFY-ROUTINE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B020-MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              IF EXIT-PATH = "GB/COMENU"
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-B020-ENTRY-MODULE.

           IF LETTER-TO-CHANGE NOT < "A" AND NOT > "Z"
              MOVE LETTER-TO-CHANGE TO SEPROFILEW
              PERFORM SET-PROFILE-SUB
              MOVE SESUB TO CHANGE-LETTER-SUB
              MOVE 3 TO ELE
              GO TO B020-GOTO-FIELD.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO = 0 )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO B020-MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       B020-ENTER-ELE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = " " )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO B020-MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO B020-CREATE-ROUTINE
              ELSE
                 IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                    GO TO B020-MODIFY-ROUTINE.

           IF ELE = 1
              MOVE "F1-CLR, ENTER THE PROGRAMS EXPIRATION DATE:" TO MESS.

           IF ELE < 3
              IF EXIT-PATH = "GB/COMENU"
                 MOVE "CHANGE THIS FLAG IN SECURITY NAME MAINTENANCE!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO B020-CREATE-ROUTINE
              ELSE
                 MOVE "THIS FIELD MAY NOT BE CHANGED!" TO MESS
                 PERFORM SEND-MESS
                 GO TO B020-CREATE-ROUTINE.

           IF ELE = 2
              MOVE "F1-CLR, IS THIS A PURCHASED OPTION (Y/N):" TO MESS.

           IF ELE = 3
              MOVE "F1-CLR, ACCESSIBLE FOR THIS PROFILE(Y/N):"
                TO MESS.
           PERFORM SEND-LEGEND.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       B020-TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       B020-TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO B020-ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       B020-GOTO-FIELD.

           IF CHANGE-LETTER-SUB > 0
              GO TO F-03  F-03B F-03C F-03D F-03E F-03F F-03G
                    F-03H F-03I F-03J F-03K F-03L F-03M F-03N
                    F-03O F-03P F-03Q F-03R F-03S F-03T F-03U
                    F-03V F-03W F-03X F-03Y F-03Z
                 DEPENDING ON CHANGE-LETTER-SUB.

           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.

           GO TO END-B020-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO SE-PURCHASABLE.

           IF NOT ( SE-PURCHASABLE = "Y" OR "N" )
              PERFORM INVALID-CODE
              GO TO B020-ENTER-ELE.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO SE-EXP-DATE.
           GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
      *                "A" PROFILE MUST BE "Y"
       F-03.
           MOVE VDUBUF TO SE-PROF(1).

           IF SE-PROF(1) NOT = "Y"
              MOVE "'A' PROFILE MUST BE 'Y'" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03B.
           MOVE "E  A01003B." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B020-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03B.
           MOVE "B" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03B.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03C.
           MOVE "E  A01003C." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03B.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03C.
           MOVE "C" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03C.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03D.
           MOVE "E  A01003D." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03C.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03D.
           MOVE "D" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03D.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03E.
           MOVE "E  A01003E." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03D.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03E.
           MOVE "E" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03E.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03F.
           MOVE "E  A01003F." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03E.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03F.
           MOVE "F" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03F.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03G.
           MOVE "E  A01003G." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03F.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03G.
           MOVE "G" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03G.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03H.
           MOVE "E  A01003H." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03G.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03H.
           MOVE "H" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03H.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03I.
           MOVE "E  A01003I." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03H.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03I.
           MOVE "I" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03I.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03J.
           MOVE "E  A01003J." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03I.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03J.
           MOVE "J" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03J.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03K.
           MOVE "E  A01003K." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03J.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03K.
           MOVE "K" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03K.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03L.
           MOVE "E  A01003L." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03K.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03L.
           MOVE "L" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03L.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03M.
           MOVE "E  A01003M." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03L.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03M.
           MOVE "M" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03M.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03N.
           MOVE "E  A01003N." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03M.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03N.
           MOVE "N" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03N.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03O.
           MOVE "E  A01003O." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03N.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03O.
           MOVE "O" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03O.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03P.
           MOVE "E  A01003P." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03O.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03P.
           MOVE "P" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03P.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03Q.
           MOVE "E  A01003Q." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03P.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03Q.
           MOVE "Q" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03Q.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03R.
           MOVE "E  A01003R." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03Q.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03R.
           MOVE "R" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03R.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03S.
           MOVE "E  A01003S." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03R.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03S.
           MOVE "S" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03S.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03T.
           MOVE "E  A01003T." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03S.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03T.
           MOVE "T" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03T.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03U.
           MOVE "E  A01003U." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03T.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03U.
           MOVE "U" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03U.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03V.
           MOVE "E  A01003V." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03U.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03V.
           MOVE "V" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03V.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03W.
           MOVE "E  A01003W." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03V.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03W.
           MOVE "W" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03W.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03X.
           MOVE "E  A01003X." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03W.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03X.
           MOVE "X" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03X.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03Y.
           MOVE "E  A01003Y." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03X.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03Y.
           MOVE "Y" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03Y.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).
           IF CHANGE-LETTER-SUB > 0
              GO TO B020-CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03Z.
           MOVE "E  A01003Z." TO VDU
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-03Y.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-03Z.
           MOVE "Z" TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE VDUBUF TO SE-PROF(SESUB).
           IF NOT ( SE-PROF(SESUB) = "Y" OR "N")
              PERFORM INVALID-CODE
              GO TO F-03Z.
           IF SE-PROF(SESUB) = " "
              MOVE "N" TO SE-PROF(SESUB).

           GO TO B020-CREATE-ROUTINE.

       END-B020-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       B030-WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-SE1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO B030-WRITE-RECORD.

           MOVE SE-REC TO CHANGED-SE-REC.

           IF ( ORIGINAL-SE-REC = CHANGED-SE-REC )
              GO TO END-B030-WRITE-MODULE.

           PERFORM READ-SE1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B030-WRITE-MODULE.

           IF ( SE-REC NOT = ORIGINAL-SE-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-B030-WRITE-MODULE.

           MOVE CHANGED-SE-REC TO SE-REC.
           PERFORM REWRITE-SE1-FILE.
           IF IO-FG = 0
              MOVE "C"            TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW.

           GO TO END-B030-WRITE-MODULE.

       B030-WRITE-RECORD.
           PERFORM WRITE-SE1-FILE.
           IF IO-FG = 0 
              MOVE SE-REC TO ORIGINAL-SE-REC
              MOVE "A"    TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
           ELSE
              PERFORM ALREADY-CREATED.

       END-B030-WRITE-MODULE.
           PERFORM CLOSE-SE1-FILE.

      ******************************************************************
       B040-DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-B040-DELETE-MODULE.

           IF EXIT-PATH NOT = "GB/COMENU"
              GO TO END-B040-DELETE-MODULE.

           PERFORM OPEN-SE1-FILE.
           PERFORM READ-SE1-FILE.

           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B040-DELETE-MODULE.

           IF SE-REC NOT = ORIGINAL-SE-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-B040-DELETE-MODULE.

           PERFORM DELETE-SE1-FILE.
           IF IO-FG = 0
              MOVE "D"            TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW.

       END-B040-DELETE-MODULE.
           PERFORM CLOSE-SE1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                     TO NEW-FLAG.
           MOVE "S  A000NEW."         TO VDU.
           MOVE "*** NEW RECORD ***"  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES       TO SE-REC.
           MOVE "N"          TO SE-PURCHASABLE.
           MOVE 0            TO SE-EXP-DATE.

           MOVE HOLD-SE1-KEY TO SE1-KEY.

           MOVE 0            TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
           MOVE SE-PURCHASABLE       TO F01.
           MOVE SE-EXP-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY        TO F02.
           PERFORM VARYING SESUB FROM 1 BY 1 UNTIL SESUB > 26
              MOVE SE-PROF(SESUB)    TO F03(SESUB)
           END-PERFORM.


           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE " ENTER DIR AND PROGRAM NAME, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM OPEN-SE1-FILE.
           PERFORM START-SE1-FILE.
       ENTER-KEY.
           MOVE "E  A060SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE SE1-KEY TO HOLD-SE1-KEY
                 MOVE SE-REC TO ORIGINAL-SE-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF  TO SE1-KEY.
           PERFORM START-SE1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-SE1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK SE-FILE.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.
           MOVE "S  A060SEFORM." TO VDU.
           MOVE SE-FORM TO VDUBUF.
           PERFORM SCREENIO.

       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-SE1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           DISPLAY SPACES UPON SEMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SEMAIN-SCREEN UPON SEMAIN-WINDOW-HANDLE.
           MOVE SEMAIN-FORM-FIELDS TO WR-BUF.
           MOVE SEMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE SEMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE SEMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/SEMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       UPDATE-AUDITW.
           MOVE FORM-PATHNAME   TO FORM-PATH.
           MOVE FORM-PROGX      TO AUDITW-PROGRAM.
           MOVE "AUDITW"        TO FORM-NAM.
           MOVE ORIGINAL-SE-REC TO AUDITW-ORIG-REC.
           MOVE SE-REC          TO AUDITW-CHG-REC.
           MOVE "SEFILE"        TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.
       SET-PROFILE-SUB.
           EVALUATE SEPROFILEW
              WHEN 'A' MOVE 1 TO SESUB
              WHEN 'B' MOVE 2 TO SESUB
              WHEN 'C' MOVE 3 TO SESUB
              WHEN 'D' MOVE 4 TO SESUB
              WHEN 'E' MOVE 5 TO SESUB
              WHEN 'F' MOVE 6 TO SESUB
              WHEN 'G' MOVE 7 TO SESUB
              WHEN 'H' MOVE 8 TO SESUB
              WHEN 'I' MOVE 9 TO SESUB
              WHEN 'J' MOVE 10 TO SESUB
              WHEN 'K' MOVE 11 TO SESUB
              WHEN 'L' MOVE 12 TO SESUB
              WHEN 'M' MOVE 13 TO SESUB
              WHEN 'N' MOVE 14 TO SESUB
              WHEN 'O' MOVE 15 TO SESUB
              WHEN 'P' MOVE 16 TO SESUB
              WHEN 'Q' MOVE 17 TO SESUB
              WHEN 'R' MOVE 18 TO SESUB
              WHEN 'S' MOVE 19 TO SESUB
              WHEN 'T' MOVE 20 TO SESUB
              WHEN 'U' MOVE 21 TO SESUB
              WHEN 'V' MOVE 22 TO SESUB
              WHEN 'W' MOVE 23 TO SESUB
              WHEN 'X' MOVE 24 TO SESUB
              WHEN 'Y' MOVE 25 TO SESUB
              WHEN 'Z' MOVE 26 TO SESUB
              WHEN OTHER PERFORM INVALID-CODE.
       GET-SE.
           PERFORM OPEN-SE1-FILE.
           PERFORM READ-SE1-FILE.
           PERFORM CLOSE-SE1-FILE.
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.
       INVALID-CODE.
           MOVE " ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-SE1-FILE.
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SEMAIN-SCREEN.
           DESTROY SEMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
