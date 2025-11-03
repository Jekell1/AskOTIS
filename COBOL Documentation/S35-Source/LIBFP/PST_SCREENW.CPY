      * COPYMEMBER: LIBFP/PST.SCREENW.CPY
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST_SCREENW.CPY
      *
      * DESCRIPTION   :  LASER (PST) SCREEN COPY LIBRARY WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2014.0429 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               **     **
      *
      ******************************************************************
      *
      *    P C L - S C R E E N - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/CHCKI_DEF.CPY".
       COPY "LIBLP/BCHKI_DEF.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".
       COPY "LIBLP/CHCKI_WKS.CPY".
       COPY "LIBLP/BCHKI_WKS.CPY".

      ******************************************************************
      *    L I N K A G E - S E C T I O N
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
      *=================================================================
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

      ******************************************************************
      *    S C R E E N - S E C T I O N
      ******************************************************************
       SCREEN SECTION.
       COPY "LIBLP/LONPM_SCN.CPY".
       COPY "LIBLP/CHCKI_SCN.CPY".
       COPY "LIBLP/BCHKI_SCN.CPY".

