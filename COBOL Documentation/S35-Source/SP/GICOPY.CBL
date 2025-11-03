       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GICOPY.
       DATE-WRITTEN.    072188.
      ******************************************************************
      *
      *                 (SP)
      *  DESCRIPTION:   GENERAL LEDGER INTERFACE COPY UTILITY
      *
      *  REV:
      *   JTG 091688 CHANGED TO HANDLE 51 FIELDS IN GIREC
      *   BAH 101189 EXPANDED TO 56 FIELDS IN GIREC
      *   BAH 060595 EXPANDED TO 101 FIELDS
      *   BLL 122195 EXPANDED TO 102 FIELDS
      *   BAH 012496 MOVE ZEROS TO GI-LN-APPROVAL-LIMIT
      *   KEC 010297 ADDED TARGET BRANCH NUMBER RANGE
      *   KEC 010797 ADDED 'F3-ALL' LOGIC TO NEW BRANCHS RANGE
      *   GLF 090397 REMOVED ALL USE OF GLOBAL
      *   JTG 121197 CHANGED TO USE ( 78  GI-ACCT-IX-MAX    VALUE 103 )
      *   BLV 051398 FIXED WK-REC TO MATCH GI; ALSO FORCED 9999999 TO
      *              GI-LIMIT FOR NOW...NEED TO ADD A FLAG TO INDICATE
      *              IF CUSTOMER IS USING APPROVALS & HANDLE CORRECTLY!
      *  KEC 080698 ADDED GP-FILE TO TEST FOR IF IT IS MERCURY SO TO
      *              MOVE ZEROES TO GI-APPROVAL-LIMIT IF IT IS, OR
      *              MOVE 9999999 IF NOT.
      *  JTG 081398 ADDED 99 F7 EXIT TO GO TO LP/LPM2MU
      *  JTG 010599 ADDED NEW GI-REC LAYOUT TO WK-REC
      *  BLV 991111 MOVE 999999 TO GI-APPROVAL-LIMIT IF NOT MERCURY,
      *             WAS ONLY DOING THIS ON A REWRITE???
      *  BLV 031203 ADD FLAG TO DETERINE HOW TO HANDLE APPROVAL LIMITS:
      *             9 = SET TO 9999999 (USED TO ALWAYS DO THIS)
      *             0 = SET TO 0 (THEN THEY WILL HAVE TO SET MANUALLY)
      *             S = SET TO WHATEVER SOURCE LIMIT IS (NEW DEFAULT)
      *             N = DON'T CHANGE TARGET LIMIT IF TARGET EXISTS,
      *                 OTHERWISE USE SOURCE
      *  BLM 100913 ADDED 2 MISSING END-IF'S ON THE CHECK FOR APPROVAL
      *             FLAG.  BUG (FROM CHANGE ABOVE) CAUSED COPY TO STOP
      *             WITH A WRITE ERROR OF 22 ON THE WKFILE (TARGET) IF
      *             THE TARGET RECORD EXISTED & USER CHOSE APPROVAL
      *             FLAG OF 9 OR 0.  ALSO CHANGED THE VALUE OF
      *             WK-ACCT-IX-MAX FROM 116 TO 123.
      * BAH 111207 REPLACED RRFORM WITH SCREEN
      * BAH 181023 GLOBAL GIFILE
      *  CS 190516 CHANGED WK-REC TO MATCH GI-REC (FUTURE FIELDS)
      * BAH 20191003 REMOVED ACCESS-CALL SOURCE-PATH, TARGET-PATH
      * BAH 20191113 REMOVED SOURCE-PATH AND TARGET-PATH
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
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/GICOPY.CBL S35 02/23/24-11:34:02 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       01  MESS-LIST       PIC X(5)  VALUE "MESS.".

       01  ERRCD           PIC X     VALUE " ".

       01  SUB             PIC 9(3).
       01  IO-TYPE         PIC X(3).

       COPY "LIBGL/GL01GI.CPY" REPLACING LEADING "GI" BY "GIOR".

       01  WK-GL-ACCOUNT   PIC 9(11).
       01  WK-GL REDEFINES WK-GL-ACCOUNT.
           03  WK-GL-BR    PIC 9(4).
           03  WK-GL-ACCT  PIC 9(7).

       01  APPROVAL-FG            PIC X.
       01  SAVE-LN-APPROVAL-LIMIT PIC 9(7) VALUE 0.

      *-----------------------------------------------------------------
       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  ORIG-BRANCH    PIC 9(4)    VALUE 0.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  OBNAME         PIC X(40)   VALUE SPACES.
       01  NBNAME-LIST1   PIC X(8)    VALUE "NBNAME1.".
       01  NBNAME-LIST2   PIC X(8)    VALUE "NBNAME2.".
       01  NBNAME         PIC X(40)   VALUE SPACES.

       01  TARGET-KEY.
           03  TARGET-BRNO   PIC 9(4).
           03  TARGET-CLASS  PIC 9(3).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       01  GICOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/GICOPY_DEF.CPY".
       COPY "LIBSP/GICOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/GICOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GICOPY"      TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GICOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-BR-FILE.

       ENTER-ORIG-BRANCH.
           MOVE "ENNN040OLDBR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ORIG-BRANCH.
           MOVE VDUNUM0 TO ORIG-BRANCH BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ORIG-BRANCH.

           MOVE BR-NAME TO OBNAME.

           MOVE OBNAME      TO WR-BUF.
           MOVE OBNAME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "Y"              TO GI-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO GI-ALL-BASE.
           MOVE BR-MACHINE       TO GI-ALL-MACHINE.
           MOVE GI-ALL-PATH      TO GI-PATH.

           PERFORM OPEN-GI1-FILE.

       ENTER-NEWBR1.
           MOVE "ENNN040NEWBR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "16" GO TO ALL-NEWBR.
           IF VDUSTATUS = "1U" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR1.
           MOVE VDUNUM0 TO NEWBR1 BR-NO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEWBR1.

           MOVE BR-NAME TO NBNAME.
           MOVE NBNAME       TO WR-BUF.
           MOVE NBNAME-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       ENTER-NEWBR2.
           MOVE "ENNN040NEWBR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR2.
           MOVE VDUNUM0 TO NEWBR2 BR-NO.

           IF NEWBR2 < NEWBR1
              GO TO ENTER-NEWBR2.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
                                            BR-NAME
              PERFORM SEND-MESS.

           MOVE BR-NAME TO NBNAME.
           MOVE NBNAME       TO WR-BUF.
           MOVE NBNAME-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO ENTER-KEYS.

       ALL-NEWBR.
           MOVE "SN N040NEWBR1." TO VDU.
           MOVE ZEROES           TO VDUNUM0 NEWBR1.
           PERFORM SCREENIO.
           MOVE "*** ALL BRANCHES ***" TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "SN N040NEWBR2." TO VDU.
           MOVE 9999             TO VDUNUM0 NEWBR2.
           PERFORM SCREENIO.
           MOVE SPACES TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-KEYS.
           PERFORM UPDATE-FORM.

           IF ( IO-TYPE = "CLR" )
              GO TO ENTER-ORIG-BRANCH.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO ENTER-KEYS.

      ******************************************************************
       UPDATE-FORM SECTION.
      ******************************************************************

       ENTER-CL1.

           MOVE SPACES TO IO-TYPE.
           MOVE "ENNN020KEY2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-CL1.

           MOVE VDUNUM0     TO GI-CLASS.
           MOVE ORIG-BRANCH TO GI-BRANCH.
           PERFORM READ-GI1-FILE.
           IF ( IO-FG = 9 )
              MOVE "INTERFACE NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CL1.

           MOVE GI-REC TO GIOR-REC.

       ENTER-CL2.
           MOVE "ENNN020KEY5." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-CL1.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-CL2.

           MOVE VDUNUM0 TO LC-CODE TARGET-CLASS.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CL2.

       ENTER-APPROVAL-FG.
           MOVE "E  A010APPROVAL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-CL2.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-APPROVAL-FG.

           IF ( VDUBUF NOT = "9" )
            IF ( VDUBUF NOT = "0" )
             IF ( VDUBUF NOT = "S" )
              IF ( VDUBUF NOT = "N" )
              GO TO ENTER-APPROVAL-FG.

           MOVE VDUBUF TO APPROVAL-FG.

       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-APPROVAL-FG.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF = "N" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM
           ELSE
           IF ( VDUBUF NOT = "Y" )
              GO TO ENTER-ACCEPT-YN.

      * COPYING OLD GI-RECORD TO NEW GI-RECORD (MUTIPLE-BRANCHS).

           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE NEWBR1 TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-BR-UPDATE-FORM.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES )
              GO TO END-UPDATE-FORM.

           IF ( BR-NO < NEWBR1 )
              GO TO NEXT-BR-UPDATE-FORM.

           IF ( BR-NO > NEWBR2 )
              GO TO END-UPDATE-FORM.

      * SKIP ORIGINATING BRANCH OR YOU WILL GET RECORD LOCKING PROBLEM
           IF BR-NO = ORIG-BRANCH
              GO TO NEXT-BR-UPDATE-FORM.

      * TARGET GI CONTROL RECORD
           MOVE BR-NO        TO GI-BRANCH TARGET-BRNO.
           MOVE TARGET-CLASS TO GI-CLASS.

           PERFORM READ-GI1-FILE.

      * NOTE, IF APPROVAL-FG = S, APPROVAL LIMIT GETS MOVED IN WITH
      * GI-REC
           IF ( IO-FG = 0 )
              MOVE GI-LN-APPROVAL-LIMIT TO SAVE-LN-APPROVAL-LIMIT
              MOVE GIOR-REC   TO GI-REC
              MOVE TARGET-KEY TO GI1-KEY
              PERFORM SET-ACCOUNTS
              IF APPROVAL-FG = "9"
                 MOVE 9999999          TO GI-LN-APPROVAL-LIMIT
              ELSE
                 IF APPROVAL-FG = "0"
                    MOVE 0             TO GI-LN-APPROVAL-LIMIT
                 ELSE
                    IF APPROVAL-FG = "N"
                       MOVE SAVE-LN-APPROVAL-LIMIT TO
                                          GI-LN-APPROVAL-LIMIT
                    END-IF
                END-IF
              END-IF
              PERFORM REWRITE-GI1-FILE
              GO TO NEXT-BR-UPDATE-FORM.

      * NOTE, IF APPROVAL-FG = S OR N, APPROVAL LIMIT GETS MOVED IN WITH
      * GI-REC

           MOVE GIOR-REC     TO GI-REC.
           MOVE TARGET-KEY   TO GI1-KEY.
           PERFORM SET-ACCOUNTS.
           IF APPROVAL-FG = "9"
              MOVE 9999999         TO GI-LN-APPROVAL-LIMIT
           ELSE
              IF APPROVAL-FG = "0"
                  MOVE 0           TO GI-LN-APPROVAL-LIMIT.

           PERFORM WRITE-GI1-FILE.
           IF ( IO-FG = 9 )
              GO TO IO-ERROR.

           GO TO NEXT-BR-UPDATE-FORM.

       END-UPDATE-FORM.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
       SET-ACCOUNTS SECTION.
      ******************************************************************
           MOVE 1 TO SUB.

       SET-NEXT.
           MOVE GIOR-BR-ACCOUNT(SUB) TO WK-GL-ACCOUNT.

           IF WK-GL-ACCT NOT = 0
              IF WK-GL-BR = ORIG-BRANCH
                 MOVE TARGET-BRNO TO WK-GL-BR
              END-IF
              MOVE WK-GL-ACCOUNT TO GI-BR-ACCOUNT(SUB).

           IF SUB < GI-ACCT-IX-MAX
              ADD 1 TO SUB
              GO TO SET-NEXT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO GICOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GICOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GICOPY-SCREEN UPON GICOPY-WINDOW-HANDLE.
           MOVE GICOPY-FORM-FIELDS TO WR-BUF.
           MOVE GICOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE GICOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE GICOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/GICOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGL/GLGI1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       ENDIT.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GICOPY-SCREEN.
           DESTROY GICOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
