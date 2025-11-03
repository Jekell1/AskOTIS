      * COPYMEMBER: LIBFP/FPFDPST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FP01PST
      *
      * DESCRIPTION   :  'FD' FOR PST-FILE
      *
      *=================================================================
      * REV:
      * KEC 2011.0923 ***NEW***
      * AMM 2021.0219 ADDED PST-SEQ-FILE
      * BAH 2023.1110 ADDED MANIFEST FILE FOR F3-ALL DOC PRINT TO FILE
      *               WITH MANIFEST #1630
      * BAH 2023.1205 ADDED MANIFEST-MAKER-DOB, MADE FILE VARIABLE #1630A
      * BAH 2024.0226 ADDED MANIFEST-COMAKER-OPT-OUT #1638A
      * BAH 2024.0821 ADDED MANIFEST-LOANDATE #1674 S35Q-119
      *
      ******************************************************************
      *
      *    P S T - F I L E
      *
      *=================================================================
      *    DESC: BASED FROM LIBGL/GLFDADP
      ******************************************************************
       FD  PST-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 500 CHARACTERS
           DEPENDING ON PST-REC-BYTE-SIZE
           LABEL RECORDS ARE STANDARD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PST-REC.
           03  PST-REC-BYTE         PIC X(01)   OCCURS 500.

      *=================================================================
       FD  PST-SEQ-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 500 CHARACTERS
           DEPENDING ON PST-REC-BYTE-SIZE
           LABEL RECORDS ARE STANDARD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PST-SEQ-REC.
           03  PST-SEQ-REC-BYTE     PIC X(01)   OCCURS 500.

      *=================================================================
       78  PST-REC-BYTE-MAX                     VALUE 500.

       FD  MANIFEST-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 1000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  MANIFEST-REC.
           03  MANIFEST-BYTE        PIC X OCCURS 1000 TIMES.

      * IF THIS CHANGES, YOU MUST RECOMPILE ALL THE PST PRINT PROGRAMS
       01  MANIFEST-PROGRAM-REC.
           03  MANIFEST-PROGRAM     PIC X(50).
           03  MANIFEST-PROGRAM-CR  PIC X.

      * THIS ONE IS ONLY USED IN LONPM0, IF YOU CHANGE, ONLY NEED TO
      * COMPILE LONPM0
       01  MANIFEST-FIRST-RECORD.
           03  MANIFEST-STATE                PIC XX.
           03  MANIFEST-COVER-ID.
               05  MANIFEST-LAST-4           PIC 9(4).
               05  MANIFEST-BR-NO            PIC 9(4).
               05  MANIFEST-MID-2            PIC 9(2).
               05  MANIFEST-LN-NUMBER        PIC 9(8).
               05  MANIFEST-FIRST-3          PIC 9(3).
           03  MANIFEST-CLASS                PIC 99.
           03  MANIFEST-SUBCLASS             PIC 99.
           03  MANIFEST-LNAMT                PIC 9(7).99.
           03  MANIFEST-OPT-OUT              PIC X.
           03  MANIFEST-MAKER-FNAME          PIC X(16).
           03  MANIFEST-MAKER-LNAME          PIC X(16).
           03  MANIFEST-MAKER-DOB            PIC 9(8).
           03  MANIFEST-COMAKER-FNAME        PIC X(16).
           03  MANIFEST-COMAKER-LNAME        PIC X(16).
           03  MANIFEST-COMAKER-OPT-OUT      PIC X.
           03  MANIFEST-LOANDATE             PIC 9(8).
           03  MANIFEST-FIRST-CR             PIC X.

