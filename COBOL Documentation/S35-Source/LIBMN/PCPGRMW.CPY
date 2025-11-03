      *A30 CONVERSION COMPLETED
      * COPYMEMBER: LIBMN/PCPGRMW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBMN/PCPGRMW
      *
      * DESCRIPTION   :  SCRATCH PAD FILE (PC) MAINTENANCE WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2006.0629 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ***NEW***
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      *
      ******************************************************************
       01  PCPGRM-LINK-BUF.
           03  ERRCD                       PIC X(01).
           03  FORM-NAME                   PIC X(20).
           03  NEW-RECORD                  PIC X(01).
           03  PAGE-SELECTION              PIC X(02).
           03  SAVEKEY.
               05  SAVEKEY-PC1-KEY.
                   07  SAVEKEY-PC-ID       PIC X(01).
                   07  SAVEKEY-PC-BRANCH   PIC 9(04).
                   07  SAVEKEY-PC-SOC-SEC  PIC 9(10).
           03  C-SYSDATE.
               05  C-SYSDATE-MM            PIC 9(02).
               05  FILLER                  PIC X(01).
               05  C-SYSDATE-DD            PIC 9(02).
               05  FILLER                  PIC X(01).
               05  C-SYSDATE-YY            PIC 9(02).
           03  PCPGRM-WINDOW-HANDLE        PIC X(10) VALUE SPACES.

      *=================================================================
       01  PROGRAM-FORM.
           03  FILLER                       PIC X(07)   VALUE "MN/PCPG".
           03  PROGRAM-FORM-PAGE            PIC X(02)   VALUE "RM".
               88  PROGRAM-FORM-PAGE-VALID      VALUE "01" "A2" "C2".
           03  FILLER             REDEFINES PROGRAM-FORM-PAGE.
               05  PROGRAM-FORM-TYPE        PIC X(01).
               05  PROGRAM-FORM-PGNO        PIC 9(01).
