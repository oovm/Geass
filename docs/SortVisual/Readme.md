# Functions

SortVisual 模块

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/SortVisual.wl"];
```

- 预加载以下函数

```
plotSort[mat_] := ArrayPlot[Transpose[mat],
  FrameTicksStyle -> Opacity[1],
  FrameTicks -> {{None, None}, {All, None}}, FrameStyle -> Opacity[0],
  AspectRatio -> 1/GoldenRatio^GoldenRatio,
  ColorFunction -> "Rainbow", ImageSize -> 600
  ]
```

## ShellSort
### 参数说明

::: tip ShellSort[list]
- list 类型为 `List`, 待排序的列表
:::

### 返回值

给出输入列表的希尔排序过程追踪


### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:


## BubbleSort
冒泡排序过程追踪

## InsertionSort
插入排序过程追踪

## CocktailSort
鸡尾酒排序过程追踪

## BogoSort

量子猴排过程追踪
