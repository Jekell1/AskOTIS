      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRREBT.
       DATE-WRITTEN.    122195.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ INSURANCE RATE FILE FOR REBATES
      *
      *                 CALLED FROM LIBGB/REBATE
      * REV:
      *  JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
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
           "@(#)CL/IRREBT.CBL S35 01/31/22-15:34:05 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  ERRCD           PIC X     VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

      **********************************
      * LINKAGE PROGRAM SECTION:
      **********************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNRB     PIC X.
           03  REB-COMMON-REBATE    PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA      PIC X(40).

           03  REB-IRREBT             REDEFINES REB-COMMON-AREA.
               05  IRREBT-REMTERM     PIC S9(5).
               05  IRREBT-INSSUB      PIC 99.
               05  IRREBT-MANYDECIMALS PIC 9(5)V9(4).
               05  FILLER             PIC X(23).
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME REB-COMMON LN-REC SP-REC EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE 0 TO IRREBT-MANYDECIMALS.
           MOVE SP-INSRATECD (IRREBT-INSSUB) TO IR-KEY.
           SUBTRACT 70 FROM IR-KEY.
           PERFORM OPEN-IR-FILE.
           PERFORM READ-IR-FILE.
           PERFORM CLOSE-IR-FILE.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF IRREBT-REMTERM NOT > 0
              GO TO END-ROUTINE.

           SET IRIX TO IRREBT-REMTERM.
           MOVE IR-INSRATE(IRIX) TO IRREBT-MANYDECIMALS.
           IF NOT (SP-INSJFAC (IRREBT-INSSUB) ZERO
                          OR (SP-INSJFAC (IRREBT-INSSUB) = 100)
                  )
              COMPUTE IRREBT-MANYDECIMALS ROUNDED =
                  IRREBT-MANYDECIMALS * SP-INSJFAC (IRREBT-INSSUB)
                  SIZE ERROR GO TO END-ROUTINE.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IR-FILE.
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPIRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
