      * COPYMEMBER: LIBSP/SPWSVCQ
      *=================================================================
      * REV:
      * JKC 2025-0324 S35Q-191 MASSIVE FIX FOR COLLECTIONS; FOR USING
      *         A IDENTICAL A15 VISION STRUCTURE FOR THE VCQFILE; FIX
      *         HANDLING DUPLICATE CQ2-KEY WITHIN THE VCQFILE WHICH YOU
      *         CAN READ LIBSP/SP01VCQ FOR MORE DETAIL INFORMATION.
      *
      * JKC 2025-0424 S35Q-200 ADDED VCQ-ALL-PATH.
      *
      ******************************************************************
       01  VCQ-PATH                        PIC X(35) VALUE SPACES.

       01  VCQ-ALL-PATH.
           03  VCQ-ALL-BASE                PIC X(9).
           03  FILLER                      PIC X(1)  VALUE "/".
           03  VCQ-ALL-MACHINE             PIC X(2).
           03  FILLER                      PIC X(7)  VALUE "/WK/VCQ".
           03  VCQ-ALL-BRNO                PIC 9(4).

