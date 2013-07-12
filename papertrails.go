package main

import (
	"compress/gzip"
	"crypto/md5"
	"flag"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"camlistore.org/pkg/misc/amazon/s3"
	"github.com/dustin/go-humanize"
)

var accessKey = flag.String("accessKey", "", "S3 access key")
var secretKey = flag.String("secretKey", "", "S3 secret key")
var bucket = flag.String("bucket", "", "S3 bucket")
var startPath = flag.String("startPath", "", "S3 list start path")
var matchPrefix = flag.String("matchPrefix", "papertrail/logs/dt=",
	"Prefix for matching files.")
var rollupPath = flag.String("rollup", "papertrail/rollup",
	"Path to hold the rolled up files")
var maxKeys = flag.Int("maxKeys", 93, "Maximum number of keys to return")

func baseDate(s string) string {
	return filepath.Base(s)[:7]
}

func exists(fn string) bool {
	_, err := os.Stat(fn)
	return err == nil
}

func s3exists(c s3.Client, fn string) bool {
	_, err := c.Stat(fn, *bucket)
	return err == nil
}

func copyFile(c s3.Client, src, dest string) (err error) {
	if exists(dest) {
		return nil
	}

	tmpfile := dest + ".tmp"

	defer func() {
		if err == nil {
			err = os.Rename(tmpfile, dest)
		} else {
			os.Remove(tmpfile)
		}
	}()

	log.Printf("Downloading %v -> %v", src, dest)
	df, err := os.OpenFile(tmpfile, os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		return err
	}
	defer df.Close()

	rc, _, err := c.Get(*bucket, src)
	if err != nil {
		return err
	}

	gzr, err := gzip.NewReader(rc)
	if err != nil {
		return err
	}
	defer rc.Close()

	_, err = io.Copy(df, gzr)
	return err
}

func mkarchive(outf string, infs []string) (err error) {
	if exists(outf) {
		return nil
	}

	tmpfile := outf + ".tmp"
	defer func(start time.Time) {
		if err == nil {
			err = os.Rename(tmpfile, outf)
		} else {
			os.Remove(tmpfile)
		}
		log.Printf("Completed archive %v in %v",
			outf, time.Since(start))
	}(time.Now())
	args := append([]string{"a", tmpfile}, infs...)
	cmd := exec.Command("7z", args...)
	cmd.Stderr = os.Stderr
	cmd.Stdout = os.Stdout
	return cmd.Run()
}

func s3upload(c s3.Client, localfile, remotefile string) error {
	if s3exists(c, remotefile) {
		return nil
	}

	defer func(start time.Time) {
		log.Printf("Completed upload %v in %v",
			remotefile, time.Since(start))
	}(time.Now())

	f, err := os.Open(localfile)
	if err != nil {
		return err
	}
	defer f.Close()

	h := md5.New()
	n, err := io.Copy(h, f)
	if err != nil {
		return err
	}

	_, err = f.Seek(0, 0)
	if err != nil {
		return err
	}

	log.Printf("Uploading %v (%x) %v", remotefile, h.Sum(nil),
		humanize.Bytes(uint64(n)))

	pr := newProgressReader(f, n)
	defer pr.Close()

	return c.PutObject(remotefile, *bucket, h, n, pr)
}

func doMonth(c s3.Client, month string, files []string) {
	log.Printf("%v", month)
	err := os.Mkdir(month, 0777)
	if err != nil && !os.IsExist(err) {
		log.Fatalf("Can't create directory %v: %v", month, err)
	}
	// defer os.RemoveAll(month)
	archivefiles := []string{}
	for _, rfn := range files {
		bn := filepath.Base(rfn)
		bn = bn[:len(bn)-3]
		dest := filepath.Join(month, bn)
		archivefiles = append(archivefiles, dest)
		for i := 0; i < 3; i++ {
			err = copyFile(c, rfn, dest)
			if err == nil {
				break
			} else {
				log.Printf("Error on %v attempt %v: %v",
					bn, i, err)
			}
		}
		if err != nil {
			log.Fatalf("Error copying: %v ", err)
		}
	}

	log.Printf("Making archive")
	montha := month + ".7z"
	err = mkarchive(montha, archivefiles)
	if err != nil {
		log.Fatalf("Error making 7z file: %v", err)
	}

	err = s3upload(c, montha, filepath.Join(*rollupPath, montha))
	if err != nil {
		log.Fatalf("Error uploading %v: %v", month, err)
	}

	for _, rfn := range files {
		log.Printf("deleting %v", rfn)
		var err error
		for i := 0; i < 3; i++ {
			err = c.Delete(*bucket, rfn)
			if err == nil {
				break
			} else {
				log.Printf("Error deleting %v on attempt %v: %v",
					rfn, i, err)
			}
		}
		if err != nil {
			log.Fatalf("Permanent deletion error of %v: %v",
				rfn, err)
		}
	}
}

func process(c s3.Client, sets map[string][]string) {
	for d, files := range sets {
		doMonth(c, d, files)
	}
}

func main() {
	flag.Parse()

	c := s3.Client{
		Auth: &s3.Auth{
			AccessKey:       *accessKey,
			SecretAccessKey: *secretKey,
		},
	}

	items, err := c.ListBucket(*bucket, *startPath, *maxKeys)
	if err != nil {
		log.Fatalf("Error listing bucket: %v", err)
	}

	end := time.Now().Format("2006-01")
	log.Printf("Ending at %v", end)

	sets := map[string][]string{}
	for _, i := range items {
		if !strings.HasPrefix(i.Key, *matchPrefix) {
			break
		}
		d := baseDate(i.Key)
		if d == end {
			break
		}
		sets[d] = append(sets[d], i.Key)
	}

	process(c, sets)
}
