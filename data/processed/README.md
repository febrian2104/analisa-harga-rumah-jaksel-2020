# Data hasil pembersihan

Folder ini berisi data hasil pembersihan dari dua workbook asli di folder
`data/`. Workbook asli tidak diubah dan kedua sumber tidak digabungkan.

## Keluaran

- `harga_rumah_jaksel_bersih.csv` dan `.rds`: data utama Jakarta Selatan.
- `data_rumah_bersih.csv` dan `.rds`: data listing dengan judul rumah.
- `audit/ringkasan_pembersihan.csv`: jumlah baris, duplikat, validitas, dan
  pencilan.
- `audit/duplikat_dihapus_*.csv`: salinan baris berulang yang dikeluarkan.

## Aturan pembersihan

- Nama kolom distandarkan ke `snake_case` dan tipe numerik diperiksa.
- Spasi berlebih pada teks dirapikan.
- Duplikat identik dikeluarkan; kolom nomor pada `DATA RUMAH.xlsx` tidak
  dipakai untuk menentukan duplikat.
- Nilai `GRS` distandarkan menjadi faktor `garasi` dengan level `tidak_ada`
  dan `ada`. Dataset yang mencatat jumlah garasi tetap mempertahankan
  `jumlah_garasi`.
- `harga_per_m2_tanah` dan `harga_per_m2_bangunan` ditambahkan.
- Nilai tidak valid dan pencilan metode IQR 1,5 ditandai dengan kolom `flag_`.
  Pencilan tidak dihapus.
- Tidak ada variabel tahun yang ditambahkan karena workbook tidak memiliki
  tanggal observasi.

Proses dapat diulang dengan menjalankan:

```bash
Rscript scripts/01_clean_data.R
```
