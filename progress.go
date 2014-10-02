package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/dustin/go-humanize"
)

const width = 80

var clearBuf = bytes.Repeat([]byte{' '}, width)

func init() {
	clearBuf[0] = '\r'
	clearBuf[width-1] = '\r'
}

type progressReader struct {
	report io.Writer
	orig   io.Reader
	total  int64
	width  int
	read   chan int
}

func newProgressReader(r io.Reader, n int64) io.ReadCloser {
	rv := &progressReader{
		report: os.Stdout,
		orig:   r,
		total:  n,
		width:  width,
		read:   make(chan int),
	}

	go rv.update()
	return rv
}

func (p *progressReader) showPercent(read int64) {
	p.clear()
	perc := float64(read*100) / float64(p.total)
	fmt.Fprintf(p.report, "%v/%v (%.1f%%)",
		humanize.Bytes(uint64(read)),
		humanize.Bytes(uint64(p.total)), perc)
	if s, ok := p.report.(interface {
		Sync() error
	}); ok {
		s.Sync()
	}
}

func (p *progressReader) Close() (err error) {
	defer func() { err, _ = recover().(error) }()
	p.clear()
	close(p.read)
	return nil
}

func (p *progressReader) clear() {
	p.report.Write(clearBuf)
}

func (p *progressReader) update() {
	t := time.NewTicker(time.Second)
	defer t.Stop()
	var read int64
	for {
		select {
		case <-t.C:
			p.showPercent(read)
		case n, ok := <-p.read:
			if !ok {
				return
			}
			read += int64(n)
		}
	}
}

func (p *progressReader) Read(b []byte) (int, error) {
	n, err := p.orig.Read(b)
	p.read <- n
	return n, err
}
