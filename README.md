# Code::Geass

[![Mathematica](https://img.shields.io/badge/Mathematica-%3E%3D10.0-brightgreen.svg)](https://www.wolfram.com/mathematica/)
[![Release Vision](https://img.shields.io/badge/release-v0.7.x-ff69b4.svg)](https://github.com/Moe-Net/Geass/releases)
[![Repo Size](https://img.shields.io/github/repo-size/Moe-Net/Geass.svg)](https://github.com/Moe-Net/Geass.git)

![Background](https://i.loli.net/2018/08/12/5b6fe5c08a9ae.png)

## ![项目简介](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/01_Introduce.png) Introduce

## ![安装方式](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/02_Install.png) Install

#### 发行版本

```Mathematica
PacletInstall["Geass","Site"->"http://m.vers.site/"]
```

#### 开发版本

- 需要安装最新的 Mathematica 以及 Wolfram Script

```bash
cd `wolframscript -code 'FileNameJoin[{$UserBaseDirectory, "Applications"}]'`
git clone https://github.com/Moe-Net/Geass.git --depth 20
```


## ![意见建议](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/03_ShowTime.png) Show Time!

### 解五次方程

<div align=center>
<img src="https://i.loli.net/2018/08/12/5b6fe5be92aca.png" alt="解五次方程"/>
</div>


- 演示代码

```Mathematica
Needs["Geass`"];
In[1]:= TschirnhausSolve[t^5-t+(11 2^(3/4))/(15 Sqrt[5])==0,Method->EllipticNomeQ];
In[2]:=
```

[更多演示](https://github.com/Moe-Net/Geass/blob/master/Resources/Full%20Examples%20Log.md)

## ![计划项目](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/04_TodoList.png) Todo List

- [x] 五次方程
  - [x] 五次方程椭圆函数解
  - [x] 五次方程梅耶尔函数解
  - [ ] 五次方程根式解
- [ ] 超幂运算
- [ ] 丢番图方程
  - [ ] 椭圆函数加法
  - [ ] 椭圆函数乘法

[更多计划](https://github.com/Moe-Net/Geass/blob/master/Resources/Full%20Todo%20Log.md)

## ![更新日志](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/05_ChangeLog.png) Change Log

| 版本号 |最近更新|
|:-----:| ---
| 0.3.0 | ???????
| Older | [查看完整记录](https://github.com/Moe-Net/Geass/blob/master/Resources/Full%20Change%20Log.md)

## ![意见建议](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/06_Ideas.png) Ideas

### 联系方式

QQ群: 1014125

## ![许可协议](https://raw.githubusercontent.com/Moe-Net/Geass/master/Resources/ico/07_License.png) License

The application is under **Mozilla Public License v2**.

©Copyright picture, icons and packages.

For all the ©Copyright see: [License Log Full](https://github.com/Moe-Net/Geass/blob/master/Resources/Full%20License%20Log.md).
