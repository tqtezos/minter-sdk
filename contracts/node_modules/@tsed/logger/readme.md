# @tsed/logger 

[![Build Status](https://travis-ci.org/TypedProject/logger.svg?branch=master)](https://travis-ci.org/TypedProject/logger)
[![Coverage Status](https://coveralls.io/repos/github/TypedProject/logger/badge.svg?branch=master)](https://coveralls.io/github/TypedProject/logger?branch=master)
[![TypeScript](https://badges.frapsoft.com/typescript/love/typescript.svg?v=100)](https://github.com/ellerbrock/typescript-badges/) 
[![Package Quality](https://npm.packagequality.com/badge/@tsed/logger.png)](https://packagequality.com/#?package=@tsed/logger)
[![npm version](https://badge.fury.io/js/logger.svg)](https://badge.fury.io/js/logger)
[![Dependencies](https://david-dm.org/TypedProject/logger.svg)](https://david-dm.org/TypedProject/logger#info=dependencies)
[![img](https://david-dm.org/TypedProject/logger/dev-status.svg)](https://david-dm.org/TypedProject/logger/#info=devDependencies)
[![img](https://david-dm.org/TypedProject/logger/peer-status.svg)](https://david-dm.org/TypedProject/logger/#info=peerDependenciess)
[![Known Vulnerabilities](https://snyk.io/test/github/TypedProject/logger/badge.svg)](https://snyk.io/test/github/TypedProject/ts-express-decorators)

> A multi channel logger written in TypeScript.

## Features

* Colored console logging to stdout or stderr,
* File appender, with configurable log rolling based on file size or date
* Configurable log message layout/patterns
* Different log levels for different log categories (make some parts of your app log as DEBUG, others only ERRORS, etc.)

Please refer to the [documentation](https://typedproject.github.io/logger/) for more details.

## Installation

```bash
npm install -g typescript
npm install @tsed/logger
npm install source-map-support
```

## Breaking change 3.x

Since v3.x, the logger is completely rewritten to support new features. This new version is inspired by the excellent project [log4js](https://github.com/nomiddlename/log4js-node/).

## Quick start

Minimalist version:

```typescript
import {$log} from "@tsed/logger";
$log.level = "debug";
$log.name = "APP";

$log.debug("Some debug messages");
```
Will be procude the following log output:
```
[2017-06-17 11:43:37.987] [DEBUG] [APP] - Some debug messages
```

Create your custom logger:
```typescript
import {Logger} from "@tsed/logger";

const logger = new Logger("loggerName");
logger.appenders
    .set({
        type: "stdout",
        levels: ["debug", "info", "trace"]
    })
    .set({
        type: "stderr",
        levels: ["fatal", "error", "warn"],
        layout: {
          type: "pattern",
          pattern: "%d %p %c %X{user} %m%n"
        }
    })
    .set({
        type: "file",
        filename: `${__dirname}/app.log`,
        layout:{
            type: "json",
            separator: ","
        }
    })
```

## Backers

Thank you to all our backers! 🙏 [[Become a backer](https://opencollective.com/tsed#backer)]

<a href="https://opencollective.com/tsed#backers" target="_blank"><img src="https://opencollective.com/tsed/tiers/backer.svg?width=890"></a>


## Sponsors

Support this project by becoming a sponsor. Your logo will show up here with a link to your website. [[Become a sponsor](https://opencollective.com/tsed#sponsor)]

## License

The MIT License (MIT)

Copyright (c) 2016 - 2018 Romain Lenzotti

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
