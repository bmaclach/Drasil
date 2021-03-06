package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class DerivedValues {
    
    public static void derived_values(InputParameters inParams) throws Exception {
        inParams.h = ((1.0 / 1000.0) * (((inParams.t == 2.5)) ? 2.16 : (((inParams.t == 2.7)) ? 2.59 : (((inParams.t == 3.0)) ? 2.92 : (((inParams.t == 4.0)) ? 3.78 : (((inParams.t == 5.0)) ? 4.57 : (((inParams.t == 6.0)) ? 5.56 : (((inParams.t == 8.0)) ? 7.42 : (((inParams.t == 10.0)) ? 9.02 : (((inParams.t == 12.0)) ? 11.91 : (((inParams.t == 16.0)) ? 15.09 : (((inParams.t == 19.0)) ? 18.26 : 21.44))))))))))));
        
        inParams.LDF = Math.pow((3.0 / 60), (7.0 / 16));
        
        if (inParams.g.equals("AN")) {
            inParams.GTF = 1;
        }
        else if (inParams.g.equals("FT")) {
            inParams.GTF = 4;
        }
        else if (inParams.g.equals("HS")) {
            inParams.GTF = 2;
        }
        
        inParams.SD = Math.sqrt((Math.pow(inParams.SD_x, 2) + (Math.pow(inParams.SD_y, 2) + Math.pow(inParams.SD_z, 2))));
        
        inParams.AR = (inParams.a / inParams.b);
        
        inParams.w_TNT = (inParams.w * inParams.TNT);
    }
}

