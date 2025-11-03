      * COPYMEMBER: LIBLP/POSTDATEW
      ****************************************************
      *                (LIBLP/POSTDATEW)
      *    POSTTIME, POSTDATE, AND USERID WORKERS.
      * REV:
      * BLV 000114 ADD REDEF OF POSTUSER FOR BRANCH
      * BAH 170313 MADE POST-DATE CCYYMMDD, NOT 00MMDDYY
      ****************************************************
       01  POST-USERID         PIC X(10).
       01  POST-USERIDX REDEFINES POST-USERID.
           03  FILLER          PIC X.
           03  POST-USERID-BR  PIC 9(4).
           03  FILLER          PIC X(5).

       01  POST-TIMEX.
           03  POST-TIME       PIC 9(6).
           03  FILLER          PIC 99.

       01  POST-DATE           PIC  9(8)  VALUE 0.
       01  FILLER            REDEFINES POST-DATE.
           05  POST-CC         PIC  9(2).
           05  POST-YY         PIC  9(2).
           05  POST-MM         PIC  9(2).
           05  POST-DD         PIC  9(2).
