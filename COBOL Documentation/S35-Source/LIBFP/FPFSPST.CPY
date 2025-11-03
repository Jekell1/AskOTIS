      * COPYMEMBER: LIBFP/FPFSPST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FPFSPST
      *
      * DESCRIPTION   :  'FS' FOR PST-FILE
      *
      *=================================================================
      * REV:
      * KEC 2011.0923 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW***
      * AMM 2021.0219 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED PST-SEQ-FILE
      * BAH 2023-1110 ADDED MANIFEST FILE FOR F3-ALL DOCUMENT PRINT TO FILE
      *               WITH MANIFEST #1630
      * BAH 2023-1205 CHANGED MANIFEST FILE TO ASSIGN OUTPUT FOR VARYING
      *               RECORD LENGTHS IN HEADER AND DETAILS #1630A
      *
      ******************************************************************
           SELECT PST-FILE ASSIGN TO PST-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT PST-SEQ-FILE ASSIGN TO PST-SEQ-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  FILE STATUS FILE-STAT.

           SELECT MANIFEST-FILE ASSIGN OUTPUT PST-PATH-MANIFEST
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
