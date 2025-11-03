      * COPYMEMBER: LIBNP/NPFDTRC
      ******************************************************************
      *
      *           (DAILY TRANSACTION RECORD)
      *
      *    NOTE: THIS IS A REDEFINITION OF LIBNP/NP01TR
      *
      *      ***********************************************
      *      ****************     NOTE    ******************
      *      ***** ANY CHANGES MADE HERE MUST ALSO BE  *****
      *      ***** MADE IN LIBNP/NPFDTRCNP             *****
      *      ***********************************************
      *
      *               TR-CODE  =  "C" 
      * REV:
      *  MJD 091798 ADDED TRC-DAY.
      *  MJD ?????? EXPANDED RECORD SIZE BY 31 BYTES.
      *  GLB ?????? REMOVED TRC-CLASS FROM KEY, ADDED TRC-NC-SEQNO,     
      *             ADJUSTED TRC-FILLER2 AND TRC-DATA REC SIZE
      *  MJD 990114 CHANGED TRC-GLUPD TO BE TRC-PAID.
      ******************************************************************
       01  TRC-REC.
           03  TRC-KEY.
               05  TRC-DAY              PIC 99.
               05  TRC-BRNO             PIC 9(4).
               05  TRC-COMMON-KEY.
                   07  TRC-CODE             PIC X.
                   07  TRC-CLASS            PIC 99.
                   07  TRC-NC-SEQNO         PIC 99.
                   07  TRC-CKNO             PIC 9(6).
                   07  TRC-SEQ              PIC 9(6).
                   07  TRC-FILLER2          PIC X(6).
           03  TRC-UPD-FLAG             PIC X.
           03  TRC-DATA.
               05  TRC-TYPE             PIC XX.
               05  TRC-DATE             PIC 9(6)     COMP-3.
               05  TRC-AMT              PIC S9(7)V99 COMP-3.
               05  TRC-PAYEE            PIC X(30).
               05  TRC-REF              PIC 9(6)     COMP-3.
               05  TRC-REASON           PIC X(30).
               05  TRC-GLDIST           OCCURS 5.
                   07  TRC-GLNO         PIC 9(11)    COMP-3.
                   07  TRC-GLAMT        PIC S9(7)V99 COMP-3.
               05  TRC-PAID             PIC X.
               05  TRC-SSNO             PIC 9(10) COMP-3.
               05  TRC-USERID           PIC X(10).
               05  TRC-POSTDATE         PIC 9(8)      COMP-3.
               05  TRC-POSTTIME         PIC 9(6)      COMP-3.
               05  TRC-VOID             PIC X.
               05  FILLER               PIC X(31).
