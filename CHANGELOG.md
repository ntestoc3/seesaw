# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 0.1.8 - 2020-12-01
### Changed 
- TableModel所有写入操作都进行加锁，防止并发数据修改引起异常

## 0.1.7 - 2020-12-01

### Fixed
- 修正测试文件中的部分bug,全部可以通过测试

### Changed
- TableModel使用自定义实现，不与DefaultTableModel兼容,以解决大量插入行造成的bug
- 修改所有测试文件，使用core.test代替lazytest

