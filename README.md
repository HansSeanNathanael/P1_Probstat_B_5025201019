# P1_Probstat_B_5025201019

## Hans Sean Nathanael - 5025201019

### 1.
a. 

![1.a](images/1_a.png)

adalah peluang X = 3 pada distribusi geometrik dengan p = 0,2 yang didapat dari fungsi dgeom(X, p)

b. 

![1.b](images/1_b.png)

rgeom() untuk menghasilkan 10000 data random distribusi geometrik dan kemudian dicari rata-rata berapa banyak data random yang digenerate bernilai 3


c. 

Berdasarkan hasil poin a dan b 

![1.c.a](images/1_a.png)

*hasil poin a*

![1.c.b](images/1_b.png)

*hasil poin b*

dapat diperhatikan bahwa hasil poin b lebih kecil dari pada hasil poin a tetapi mendekati hasil poin a dengan perbedaan 0.023

d. 

![1.d](images/1_d.png)

Histogram didapat dari random 10000 data poin b, didapatkan frekuensi X = 3 adalah 1001.

e.

![1.e.mv](images/1_e_mean_variance.png)


![1.e](images/1_e.png)

Rata-ratanya adalah menemui 4 orang yang tidak menghadiri acara vaksinasi sebelum keberhasilan pertama dan variansinya adalah 20

### 2.
a.

![2.a](images/2_a.png)

adalah peluang 4 pasien sembuh dari 20 pasien dengan p = 0,2. Fungsi dbinom() untuk mendapatkan nilai peluang.

b.

![2.b](images/2_b.png)

adalah histogram peluang jumlah pasien yang sembuh dengan grafik x adalah jumlah pasien sembuh dan y adalah peluangnya.

c.

![2.c.mv](images/2_c_mean_variance.png)

![2.c](images/2_c.png)

rata-rata terdapat 4 pasien yang sembuh dengan variansi 3,2

### 3.
a.

![3.a](images/3_a.png)

adalah nilai peluang 6 bayi akan lahir di rumah sakit tersebut besok. Nilai didapatkan dari fungsi dpois()

b.

![3.b](images/3_b.png)

![3.b](images/3_b_hist.png)

adalah total hari dalam 1 tahun di mana pada hari tersebut lahir 6 bayi, totalnya adalah 45 hari

c. 

![3.c](images/3_c.png)

dapat diperhatikan bahwa hasil poin b lebih kecil dari pada hasil poin a tetapi mendekati hasil poin a dengan perbedaan 0.048

d.

![3.d.mv](images/3_d_mean_variance.png)

![3.d](images/3_d.png)

Nilai rata-rata telah diketahui dari soal yaitu 6 dan variansi Poisson sama dengan rata-ratanya. Rata-rata dan variansi tidak diambil dari hasil generate random karena tidak ada keterangan harus mengambil dari hasil generate.

### 4.
a.

![4.a.pdf](images/4_a_pdf.png)

PDF chi square

![4.a.function probability](images/4_a_function_probability.png)

Peluang distribusi Chi Square

b.

![4.b](images/4_b.png)

![4.b.hist](images/4_b_hist.png)

histogram 100 data random chi square dengan degree of freedom = 10

c.

![4.d.mv](images/4_c_mean_variance.png)

![4.d](images/4_c.png)

rata-rata sama dengan degree of freedom dan variansinya adalah 2 kali degree of freedom

### 5.
a.

![5.a.pdf](images/5_a_pdf.png)

PDF distribusi exponensial dengan asumsi lambda adalah rate

![5.a.pdf](images/5_a_function_probability.png)

Peluang distribusi exponensial 

b.

![5.b.10](images/5_b_10.png)

adalah histogram distribusi exponensial 10 data random

![5.b.100](images/5_b_100.png)

adalah histogram distribusi exponensial 100 data random

![5.b.1000](images/5_b_1000.png)

adalah histogram distribusi exponensial 1000 data random

![5.b.10000](images/5_b_10000.png)

adalah histogram distribusi exponensial 10000 data random

c.

![5.c](images/5_c.png)

rata-rata dan variansi distribusi exponential dari 100 data random

### 6.
a.

![6.a.pdf](images/6_a_pdf.png)

PDF distribusi normal

![6.a.pdf](images/6_a_function_probability.png)

Peluang distribusi normal

![6.a](images/6_a.png)

Z1 didapat dari nilai terkecil hasil generate dan Z2 dari nilai terbesar generate. Untuk plot, rentang angka dari nilai terkecil hingga terbesar dari hasil generate dan dibagi membagi menjadi 50 titik diantaranya, frekuensi dari sebuah titik adalah banyaknya nilai hasil generate yang berada di antara titik tersebut dengan titik sebelumnya.

b.

![6.a](images/6_b.png)

Histogram hasil generate 100 data dengan seed(0) dan breaks 50

c.

![6.a](images/6_c.png)

Variansi dari hasil generate
