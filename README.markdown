# papertrail log rollups

I use [papertrail][papertrail] to aggregate a bunch of my logs.  They
dump lots of that into an S3 bucket once per day.

This is a tool I use to combine all my day archives into a highly
compressed month archive.

    Usage of ./papertrails:
      -accessKey="": S3 access key
      -bucket="": S3 bucket
      -matchPrefix="papertrail/logs/dt=": Prefix for matching files.
      -maxKeys=93: Maximum number of keys to return
      -progressDown=false: Display progress on downloads
      -progressUp=true: Display progress on uploads
      -rollup="papertrail/rollup": Path to hold the rolled up files
      -secretKey="": S3 secret key
      -startPath="": S3 list start path
