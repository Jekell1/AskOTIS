      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRREAD.
       DATE-WRITTEN.    122892.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ INSURANCE RATE FILE
      *
      *                 CALLED FROM CALCZL.C & CALCZ3.C
      * REV:
      *  JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 20230110 WHEN IO-BAD, IT MOVED 0 TO KEY BUT DIDNT MOVE THE
      *               THE IR-REC BACK TO THE PASSED COPY IRX-REC, DO THIS
      *               ON A BAD READ ALSO.
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
           "@(#)CL/IRREAD.CBL S34 05/24/21-13:13:27 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  ERRCD           PIC X     VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

      **********************************
      * LINKAGE PROGRAM SECTION:
      **********************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01IR.CPY" REPLACING LEADING "IR" BY "IRX".

       PROCEDURE DIVISION
             USING FORM-PATHNAME IRX-REC EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP IO-FG.

           SUBTRACT 70 FROM IRX-KEY GIVING IR-KEY.
           PERFORM OPEN-IR-FILE.
           PERFORM READ-IR-FILE.
           PERFORM CLOSE-IR-FILE.
           IF IO-BAD
              MOVE 0 TO IR-KEY.
           MOVE IR-REC TO IRX-REC.

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
