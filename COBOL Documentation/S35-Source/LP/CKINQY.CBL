       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKINQY.
       DATE-WRITTEN.    12/10/2021.
      *********************************************************
      *                 (LP)
      *
      *  DESCRIPTION:   CHECK MASTER FILE INQUIRY
      *
      * REV:
      *  20211210 BAH  NEW #1530
      *  BAH 20230908 REMOVED PA-FILE
      *********************************************************
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
      *    W O R K I N G - S T O R A G E
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/CKINQY.CBL S34 10/22/21-14:42:24 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".

       01  HOLD-CK1-KEY.
           03  HOLD-CK-BRNO            PIC 9(4).
           03  HOLD-CK-NO              PIC 9(6).

       01  ERRCD                       PIC X      VALUE SPACES.

       01  CKINQY-WINDOW-HANDLE        PIC X(10).
       01  IO-TYPE                     PIC X(3).
       01  FIRST-TIME                  PIC 9       VALUE 0.

       01  MESS-LIST                   PIC X(5) VALUE "MESS.".

       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
       01  DISP-GL.
           03  DISP-BR     PIC 9(4).
           03  FILLER      PIC X VALUE "-".
           03  DISP-NO     PIC 9(7).

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                 VALUE SPACES.
           03  F01                     PIC XX.
           03  F02                     PIC 99/99/99.
           03  F03                     PIC Z,ZZZ,ZZZ.99-.
           03  F04                     PIC X(30).
           03  F05                     PIC 9(6).
           03  F06                     PIC X(30).
           03  F07                     PIC X(12).
           03  F08                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F09                     PIC X(12).
           03  F10                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F11                     PIC X(12).
           03  F12                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F13                     PIC X(12).
           03  F14                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F15                     PIC X(12).
           03  F16                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F17                     PIC X.
           03  F171                    PIC X(9).
           03  F18                     PIC 99/99/99   BLANK ZERO.
           03  F19                     PIC X.
           03  F20                     PIC 99.
           03  F21                     PIC Z(3)9.99.
           03  F22                     PIC XX.
           03  F23                     PIC 9.
           03  F24                     PIC Z(8).
           03  F25                     PIC 999/99/9999  BLANK ZERO.
           03  F25-SOC-X-OUT REDEFINES F25.
               05  D-SOC1    PIC X(3).
               05  FILLER    PIC X.
               05  D-SOC2    PIC XX.
               05  FILLER    PIC X.
               05  D-SOC3    PIC X(4).
           03  F26                     PIC 9(10)    BLANK ZERO.
           03  F27                     PIC X.
           03  F28                     PIC X.
           03  F29                     PIC 99/99/99 BLANK ZERO.
           03  F30                     PIC 99.
           03  F31                     PIC XX.
           03  F32                     PIC ZZZZZ9.
           03  F321                    PIC X(25).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
               "010 020 030 040 050 060 070 080 090 100 110 120 130 ".
           03  FILLER      PIC X(52) VALUE
               "140 150 160 170 171 180 190 200 210 220 230 240 250 ".
           03  FILLER      PIC X(32) VALUE
               "260 270 280 290 300 310 320 321.".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CKSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CKINQY_DEF.CPY".
       COPY "LIBLP/CKINQY_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CKINQY_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
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

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "CKINQY"                   TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS CKINQY-WINDOW-HANDLE.

           PERFORM OPEN-BR-FILE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE 0 TO HOLD-CK-BRNO.

           PERFORM ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *******************************************
       ENTER-KEYS-MODULE SECTION.
      *******************************************
       ENTER-BRANCH.
           MOVE SPACES TO IO-TYPE.

           MOVE "SNNN040BRNO."   TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0
                                      BR-NO
                                      HOLD-CK-BRNO CK-BRNO.
           PERFORM SCREENIO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
               MOVE "INVALID BRANCH" TO MESS
               PERFORM SEND-MESS
               MOVE "END" TO IO-TYPE
               GO TO END-ENTER-KEYS-MODULE.

           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-CHECK.
           MOVE "F1-BRANCH, F6-SCAN WINDOW, F5-SCAN" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS      TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "ENNN060CHECK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCAN
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM CKFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-CHECK
              ELSE
                 GO TO READ-CHECK-FILE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK.
           IF VDUNUM0 = 0
              GO TO ENTER-CHECK.
           MOVE VDUNUM0 TO CK-NO.

       READ-CHECK-FILE.
           MOVE MESS-LIST TO WR-LIST.
           MOVE SPACES    TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE CK1-KEY TO HOLD-CK1-KEY.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE MESS-LIST       TO WR-LIST
              MOVE "INVALID CHECK" TO WR-BUF
              PERFORM CALL-WRFORM
              GO TO ENTER-CHECK.

       END-ENTER-KEYS-MODULE.
           EXIT.

      *******************************************
       ENTER-SCAN SECTION.
      *******************************************

           MOVE " ENTER CHECK #   OR   F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-CK1-FILE.
 
           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE 0             TO CK-NO.

           PERFORM START-CK1-FILE.

           GO TO READ-FILES-NEXT.

      *-----------------------------------------------------------------
       ENTER-KEY.
           MOVE "EN N060SCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              GO TO CLOSE-SCAN.

           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE VDUNUM0       TO CK-NO.

           PERFORM START-CK1-FILE.

      *-----------------------------------------------------------------
       READ-FILES-NEXT.
           PERFORM READ-CK1-FILE-NEXT.
      *    UNLOCK CK-FILE.
           IF IO-FG = 9 OR (CK-BRNO NOT = CK-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N060CHECK." TO VDU.
           MOVE CK-NO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.

       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       END-SCAN.
           PERFORM CLOSE-CK1-FILE.

      ***************************************
       CKFILE-SCAN SECTION.
      ***************************************
    
           MOVE "Y"          TO CKSCAN-NOSTOP-FG.
           MOVE HOLD-CK-BRNO TO CKSCANW-BRNO.
           MOVE 0            TO CK-NO.
           MOVE "WI/CKSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CKSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE CKSCAN-CK1-KEY TO CK1-KEY
              MOVE "SN N040BRNO." TO VDU
              MOVE CK-BRNO TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N060CHECK." TO VDU
              MOVE CK-NO TO VDUNUM0
              PERFORM SCREENIO.

      ***************************************************
       ENTRY-MODULE SECTION.
      ***************************************************
           MOVE SPACES TO IO-TYPE.

           MOVE MESS-LIST       TO WR-LIST.
           MOVE "F7-CLEAR, F7-EXIT" TO WR-BUF.
           PERFORM CALL-WRFORM.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           GO TO MODIFY-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE CK-TYPE     TO F01.
           MOVE CK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE CK-AMT    TO F03.
           MOVE CK-PAYEE  TO F04.
           MOVE CK-REF    TO F05.
           MOVE CK-REASON TO F06.
           IF CK-GLAMT(1) NOT = 0
              MOVE CK-GLNO(1) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL     TO F07
              MOVE CK-GLAMT(1) TO F08.
           IF CK-GLAMT(2) NOT = 0
              MOVE CK-GLNO(2) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL     TO F09
              MOVE CK-GLAMT(2) TO F10.
           IF CK-GLAMT(3) NOT = 0
              MOVE CK-GLNO(3) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL     TO F11
              MOVE CK-GLAMT(3) TO F12.
           IF CK-GLAMT(4) NOT = 0
              MOVE CK-GLNO(4) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL     TO F13
              MOVE CK-GLAMT(4) TO F14.
           IF CK-GLAMT(5) NOT = 0
              MOVE CK-GLNO(5) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL     TO F15
              MOVE CK-GLAMT(5) TO F16.

           MOVE CK-VOID        TO F17.
           IF CK-VOID = "V"
              MOVE "VOIDED"    TO F171
           ELSE
           IF CK-VOID = "R"
              MOVE "REPRINTED" TO F171
           ELSE
              MOVE SPACES      TO F171.
           MOVE CK-VOID-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F18.

           MOVE CK-PAID   TO F19.
           MOVE CK-CLASS  TO F20.
           MOVE CK-DLNO   TO F21.
           MOVE CK-PGM    TO F22.
	   MOVE CK-DLRVNO TO F23.
           MOVE CK-ACCTNO TO F24.
           IF CK-SSNO > 0
              MOVE CK-SSNO TO F25
              INSPECT F25 REPLACING ALL "/" BY "-"
              MOVE ALL "X" TO D-SOC1 D-SOC2.

           IF CK-BATCHID > 0
              MOVE CK-BATCHID    TO F26.
           MOVE CK-UNPAID        TO F27.
           MOVE CK-CLEARED-FG    TO F28.
           MOVE CK-CLEARED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F29.

           MOVE CK-TILLNO        TO F30.
           MOVE CK-EVENT-CD      TO F31.
           MOVE CK-PAYEE-NO      TO F32.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME   TO CKINQY-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO CKINQY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CKINQY-SCREEN UPON CKINQY-WINDOW-HANDLE.
           MOVE CKINQY-FORM-FIELDS TO WR-BUF.
           MOVE CKINQY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************
           MOVE CKINQY-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKINQY-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBLP/CKINQY_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
      *
      *    E N D - R O U T I N E
      *
      ******************************************************************
       END-ROUTINE SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CKINQY-SCREEN.
           DESTROY CKINQY-WINDOW-HANDLE.
           EXIT PROGRAM.
