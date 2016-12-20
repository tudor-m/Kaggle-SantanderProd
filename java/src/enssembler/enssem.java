package enssembler;

import java.awt.List;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import com.sun.javafx.collections.MappingChange.Map;

public class enssem {

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		// Data:
		// each Prediction is a
		// map (customer_id,
		// 		 map(product_id,score)
		//		)
		// reorganized as a table with fields:
		// customer_id, score_product_1, score_product_2, etc, _22:
		// map (customer_id, array of scores)
		String[] tf = {"ind_ahor_fin_ult1", "ind_aval_fin_ult1", "ind_cco_fin_ult1", "ind_cder_fin_ult1", "ind_cno_fin_ult1",
		               "ind_ctju_fin_ult1", "ind_ctma_fin_ult1", "ind_ctop_fin_ult1", "ind_ctpp_fin_ult1", "ind_deco_fin_ult1",
		               "ind_deme_fin_ult1", "ind_dela_fin_ult1", "ind_ecue_fin_ult1", "ind_fond_fin_ult1", "ind_hip_fin_ult1",
		               "ind_plan_fin_ult1", "ind_pres_fin_ult1", "ind_reca_fin_ult1", "ind_tjcr_fin_ult1", "ind_valo_fin_ult1",
		               "ind_viv_fin_ult1", "ind_nomina_ult1", "ind_nom_pens_ult1", "ind_recibo_ult1"};
		HashMap<String,Integer> fm = new HashMap<>(); // target features
		for (int i=0;i<tf.length;i++)
		{
			fm.put(tf[i], i);
		}
		
		FileWriter fw = new FileWriter("app.out.csv");
		fw.write("added_products,ncodpers\n");
		int maxFiles=20;
		FileReader[] fr_list = new FileReader[maxFiles];
		Double[] w_list = new Double[maxFiles];   // weight list
		Integer nfiles = 0;
		FileReader appcfg = new FileReader("app.cfg");
		BufferedReader b1 = new BufferedReader(appcfg);
		String st1;
		int ii=0;
		while((st1 = b1.readLine()) != null)
		{
			String st1s[] = st1.split(",");
			fr_list[ii] = new FileReader(st1s[1]);
			w_list[ii] = Double.parseDouble(st1s[0]);
			nfiles++;
			ii++;
		}
		ArrayList<Integer> cust_list = new ArrayList<>();
		
		String st;
		String[] sts;
		String[] sts2;
		//Double[] feat_score;
		Integer[] feat_score = new Integer[tf.length];

		HashMap[] mp_list = new HashMap[fr_list.length]; // score maps
		for (int j =0;j<nfiles;j++)
		{
			System.out.println("Process file "+(1+j));
			FileReader fr = fr_list[j];
			BufferedReader br = new BufferedReader(fr);
			//String st;
			HashMap<Integer,Integer[]> mp = new HashMap<>(950000);
			while((st = br.readLine()) != null)
			{
				int cod = 1;
				//String[] sts = st.split(",");
				sts = st.split(",");
				int cust_id = 0;
				try {cust_id = Integer.parseInt(sts[1]);}
				catch (NumberFormatException e)
					{cod = 0;}
				if (cod == 0) continue;
				//String[] sts2 = sts[0].split(" ");
				sts2 = sts[0].split(" ");
				//Double[] feat_score = new Double[tf.length];
				//feat_score = new Double[tf.length];
				Arrays.fill(feat_score, 0);
				for (int i=0;i<sts2.length;i++)
				{
					feat_score[fm.get(sts2[i])] = 10-i;
				}
				mp.put(cust_id,Arrays.copyOf(feat_score,feat_score.length));
				if (j==0)
					cust_list.add(cust_id);
				//System.out.println(cust_id);
				//System.out.println(sts2[0]);
			}
			mp_list[j] = mp;
			br.close();
			fr.close();
			System.gc();
		}
		//HashMap<Integer,Double[]> mp_comb = new HashMap<>();
		// Combine the maps:
		System.out.println("Process the map combinaton");
		for (int kkk=0;kkk<cust_list.size();kkk++)
		{
			Integer tmpCust = cust_list.get(kkk);
			Double[] f_score = new Double[tf.length];
			Arrays.fill(f_score, 0.);
			//Integer tmpi=0;
			for (int i=0;i<tf.length;i++)
			{
				for (int j=0;j<nfiles;j++)
				{
					f_score[i]+= ((Integer[])mp_list[j].get(tmpCust))[i] *w_list[j];
				}
				f_score[i] = f_score[i]/nfiles*(1+0.0*(Math.random()-0.5));
			}
			//mp_comb.put(tmpCust, f_score);
			// Sort the score list:
			Double[] f_sorted = Arrays.copyOf(f_score,f_score.length);
			Arrays.sort(f_sorted);
			for (int kk=f_sorted.length-1;kk>0;kk--)
			{
				//int idx = Arrays.binarySearch(f_score,f_sorted[kk]);
				int idx=0;
				if (f_sorted[kk]>0)
				{
					for (int ij=0;ij<f_score.length;ij++)
					{
						if(f_score[ij] == f_sorted[kk])
							{idx = ij;break;}
					}
					fw.write(tf[idx]+" ");
				}
			}
			fw.write(",");
			fw.write(tmpCust.toString());
			fw.write("\n");
//			for (int k=f_sorted.length-1;k>=0;k--) System.out.print(f_sorted[k]);
		}
		fw.close();
		
		System.out.println("Program end");
	}
}
