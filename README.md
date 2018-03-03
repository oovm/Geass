# Geis — Code::Geass

[![Mathematica](https://img.shields.io/badge/Mathematica-%3E%3D10.0-brightgreen.svg)](https://www.wolfram.com/mathematica/)
[![Release Vision](https://img.shields.io/badge/release-v0.7.x-ff69b4.svg)](https://github.com/GalAster/Geis/releases)
[![Repo Size](https://img.shields.io/github/repo-size/GalAster/Geis.svg)](https://github.com/GalAster/Geis.git)

![Background](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/pic/MainPage.png)

## ![项目简介](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/01_Introduce.png) Introduce

## ![安装方式](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/02_Install.png) Install

#### 手动安装

- 点击release下载最新的[安装包](https://github.com/GalAster/Geis/releases) [![Build Status](https://travis-ci.org/GalAster/Geis.svg?branch=master)](https://travis-ci.org/GalAster/Geis) 

  - 已启用 Travis CI, release 始终和 master 分支同步 

  - release 版本暂时不带说明文档

- 解压到任意`$Path`路径下

  - 一个常用的路径是`"C:\Users\{yourname}\AppData\Roaming\Mathematica\Applications"`.

  - 如果选择手动安装那么升级也要手动升级(暂时)
  
  - 删除原来的文件换上新文件即可

#### 自动安装

- 使用``BTools` ``的部署功能

## ![意见建议](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/03_ShowTime.png) Show Time!

### 解五次方程

<div align=center>
<img src="https://raw.githubusercontent.com/GalAster/Geis/master/Resources/pic/HermiteSolve.png" alt="解五次方程"/>
</div>


- 演示代码

```Mathematica
Needs["Geis`"];
In[1]:= TschirnhausSolve[t^5-t+(11 2^(3/4))/(15 Sqrt[5])==0,Method->EllipticNomeQ];
In[2]:=
```

[更多演示](https://github.com/GalAster/Geis/blob/master/Resources/Full%20Examples%20Log.md)

## ![计划项目](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/04_TodoList.png) Todo List

- [x] 五次方程
  - [x] 五次方程椭圆函数解
  - [x] 五次方程梅耶尔函数解
  - [ ] 五次方程根式解
- [ ] 超幂运算
- [ ] 丢番图方程
  - [ ] 椭圆函数加法
  - [ ] 椭圆函数乘法

[更多计划](https://github.com/GalAster/Geis/blob/master/Resources/Full%20Todo%20Log.md)

## ![更新日志](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/05_ChangeLog.png) Change Log

| 版本号 |最近更新|
|:-----:| --- 
| 0.3.0 | ???????
| Older | [查看完整记录](https://github.com/GalAster/Geis/blob/master/Resources/Full%20Change%20Log.md)

## ![意见建议](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/06_Ideas.png) Ideas

### 联系方式

|知乎主页|QQ群聊| 
|:-:|:-:|
|[<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Logo_Zhihu.png" alt="知乎链接" width = "100" align=center />](https://www.zhihu.com/people/GalAster)|[<img src="https://raw.githubusercontent.com/GalAster/Deus/master/Resources/pic/Logo_QQ.png" alt="QQ链接" width = "100" align=center />](https://jq.qq.com/?_wv=1027&k=5BqFya1)|


## ![许可协议](https://raw.githubusercontent.com/GalAster/Geis/master/Resources/ico/07_License.png) License

The application is under **Mozilla Public License v2**.

©Copyright picture, icons and packages.

For all the ©Copyright see: [License Log Full](https://github.com/GalAster/Geis/blob/master/Resources/Full%20License%20Log.md).