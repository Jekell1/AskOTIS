      * COPYMEMBER: LIBGB/SAFECKW
      * 050810 BAH ADDED COPY OF SPANISH/PESO COPY MEMBERS FOR WORLD
      * AMM 2014.0822 PR#4570 MULLEN/KSTREET; PER JUDY'S EMAIL THEY NEED
      *       TO ACCOMMODATE A LONGER ALP LINE FOR MILLION DOLLAR CHECKS
      *       ADDED ALP-LINE-X  TO SAFECKW  PIC X(95)
      ************************************************
      *            SAFECK WORKERS
      ************************************************
       01  SINGLE-DOLLARS.
           03  FILLER                  PIC X(45) VALUE
           "ONE      TWO      THREE    FOUR     FIVE     ".
           03  FILLER                  PIC X(45) VALUE
           "SIX      SEVEN    EIGHT    NINE     TEN      ".
           03  FILLER                  PIC X(45) VALUE
           "ELEVEN   TWELVE   THIRTEEN FOURTEEN FIFTEEN  ".
           03  FILLER                  PIC X(36) VALUE
           "SIXTEEN  SEVENTEENEIGHTEEN NINETEEN ".
       01  SING-DOLS REDEFINES SINGLE-DOLLARS.
           03  ALP-1-DOLS OCCURS 19    PIC X(9).
 
       01  DOUBLE-DOLLARS.
           03  FILLER                  PIC X(56) VALUE
           "TWENTY THIRTY FORTY  FIFTY  SIXTY  SEVENTYEIGHTY NINETY ".
       01  DOUB-DOLS REDEFINES DOUBLE-DOLLARS.
           03  ALP-2-DOLS OCCURS 8     PIC X(7).
      *************************************************
 
       01  DOLLAR-BUF.
           03  NUM-DOLLARS                 PIC 9(8)V99.
           03  NUM-DOLS REDEFINES NUM-DOLLARS.
               05  ND-M                    PIC 9(2).
               05  ND-A                    PIC 9(3).
               05  FILLER REDEFINES ND-A.
                   07  ND-B                PIC 9.
                   07  ND-C.
                       09  FILLER          PIC 9.
                       09  ND-D            PIC 9.
                   07  ND-E REDEFINES ND-C PIC 9(2).
               05  ND-F                    PIC 9(3).
               05  ND-CENTS                PIC 99.
           03  ALP-LINE                    PIC X(80).
           03  ALP-LINEX                   PIC X(95).
 
      *************************************************
 
       01  NEXT-3                      PIC 9 VALUE 0.
       01  ND-SUB                      PIC 9.
       01  ALP-DOLLARS                 PIC X(10).
       01  CENTS-MSG.
           03  FILLER                  PIC X(2) VALUE "& ".
           03  D-CENTS                 PIC 99.
           03  FILLER                  PIC X(4) VALUE "/100".
      *************************************************
