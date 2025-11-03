       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ELREAD.
       DATE-WRITTEN.    060592.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ CODE FILE AND PASS CD-EL-INFO
      *
      *                 CALLED FROM LATE CHARGE ROUTINES
      *                  - LPLCAP
      *                  - LPLCAS
      *                  - LPLCHG
      *
      *    NOTE:
      *          AN INVALID READ CAUSES SP-LC-CODE TO = 0
      *          CAUSING NO LATE CHARGES TO BE TAKEN
      * REV:
      *  JTG 050797 ADDED PASS OF SP-LCUNITPER-CD
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *
      * KEC 2016.0321 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED HOLD/SAVE KEYS & RECS TO MATCH CD-REC.
      * BAH 2017.1117 FIXED SP-LC-INFO, SIZES
      * BAH 190122 SPLIT CDFILE
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR SP-LC-CODEX; UNUSED
      * KEC 230824 ADDED `MOVE CD-EL-CODE TO SP-LC-CODE` IN THE
      *            ELSE CONDITION;
      *            THE `MOVE CD-EL-INFO TO SP-LC-INFO` IS NOT AN
      *            EQUAL MATCH SINCE THERE IS NO CD-ELCODE WITHIN
      *            CD-EL-INFO THUS MAKING CD-LC-CODE SPACES UPON
      *            RETURNING BACK TO THE CALLING PROGRAM.
      *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/ELREAD.CBL S35 08/24/23-13:52:25 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  ERRCD               PIC X         VALUE SPACES.
       01  FORM-PATHNAME       PIC X(22)     VALUE
                                            "/USR/A32/CO/CL/ELREAD".
       01  FORM-PATH          REDEFINES FORM-PATHNAME.
           03  FILLER          PIC X(9).
           03  FORM-OBJ        PIC XX.
           03  FILLER          PIC X.
           03  FORM-NAM.
               05  FILLER      PIC X(3).
               05  FORM-PROGX  PIC X(6).
           03  FILLER          PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  EXIT-PATHNAME PIC X(22).

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       LINKAGE SECTION.
      *----------------------------------------------------
      *    LATE CHARGE FIELDS     (22 BYTES)
      *     NOTE:
      *          THIS AREA MAPS THE CD-REC FIELD CD-EC-INFO
      *          IF SP-LC-INFO CHANGES YOU  M U S T  CHANGE
      *          THE INFORMATION IN CD-EL-INFO.
      *----------------------------------------------------
       01  SP-LC-INFO.
           03  SP-LCGRACE          PIC 9(2).
           03  SP-LCGRACECD        PIC X.
           03  SP-LCTYPE           PIC XX.
           03  SP-LCRATE           PIC 9(4)V9(4) COMP-3.
           03  SP-LCFRMLA          PIC XX.
           03  SP-LCMINCHG         PIC 99V99     COMP-3.
           03  SP-LCMAXAMT         PIC 9(4)V99   COMP-3.
           03  SP-LCDELFAC         PIC 9(3)      COMP-3.
           03  SP-LCYRTYPE         PIC 9(4)      COMP-3.
           03  SP-LC-CODE          PIC 99.
       01  SP-LCUNITPER-CD         PIC X.


       PROCEDURE DIVISION
             USING SP-LC-INFO SP-LCUNITPER-CD.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM OPEN-CD1-FILE.

           MOVE "EL"          TO CD-TYPE.
           MOVE SP-LC-CODE    TO CD-EL-CODE.
           MOVE SPACES        TO CD-EL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE 0               TO SP-LC-CODE
           ELSE
              MOVE CD-EL-INFO      TO SP-LC-INFO
              MOVE CD-EL-CODE      TO SP-LC-CODE
              MOVE CD-ELUNITPER-CD TO SP-LCUNITPER-CD.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
