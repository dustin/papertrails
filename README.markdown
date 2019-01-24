# papertrail log rollups

I use [papertrail][papertrail] to aggregate a bunch of my logs.  They
dump lots of that into an S3 bucket once per day.

This is a tool I use to combine all my day archives into a highly
compressed month archive.

    Usage: papertrails [--bucket ARG] [--input-prefix ARG] [--rollup-prefix ARG]
      Process papertrail logs.

    Available options:
      --bucket ARG             S3 bucket (default: BucketName "logarchive")
      --input-prefix ARG       prefix to input files (default: "papertrail/logs/")
      --rollup-prefix ARG      prefix to output
                               files (default: "papertrail/rollup/")
      -h,--help                Show this help text

[papertrail]: http://papertrailapp.com/
