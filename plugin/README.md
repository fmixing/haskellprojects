# o-clock solving KnownRat constraint plugin


## Overview
Тайп-чекинг плагин для компилятора ghc, который позволяет в рантайме для библиотеки  [`o-clock`](https://github.com/serokell/o-clock) проверять, что если `KnownRat a, KnownRat b`, то `KnownRat (DivRat a b)`.

## Как запускать

Для тестирования плагина в файле `Spec.hs` нужно написать {-# OPTIONS_GHC -fplugin=OClockPlugin #-}, и код, который будет проверять правильную проверку констрейнта, например:

  
```haskell ignore
test ::  forall a b . (KnownRat a, KnownRat b) =>  RatioNat
test = ratVal @(DivRat a b)

main ::  IO ()
main = print $ test @(5  /  1) @(7  /  1)
``` 

## Что происходит

 Любые плагины к компилятору ghc в хаскеле должны экспортить функцию `plugin :: Plugin`, и type-checking plugins должны возвращать плагин типа `TcPlugin` (tc for type checking).

```haskell ignore
data TcPlugin = forall s . TcPlugin
  { tcPluginInit  :: TcPluginM s
  , tcPluginSolve :: s -> TcPluginSolver
  , tcPluginStop  :: s -> TcPluginM ()
  }
```

`tcPluginInit` -- функция, в которой проводятся "подготовительные работы", например, поиск в окружении необходимых для дальнейшей работы определений.
`tcPluginSolve` -- функция, которая разрешает констрейнты.
`tcPluginStop` -- функция, в которой происходит освобождение всех ресурсов и в принципе очистка состояния. 

Поиск необходимых определений в данном плагине происходит в функции `lookupRatTyCon :: TcPluginM  RatDefs`. В ней ищется модуль, в котором лежат классы `Rat`, `KnownRat`, `DivRat` и необходимый для coersion класс (и соответствующий ему инстанс) `KnownDivRat` (он находится в модуле плагина), type constructor-ы этих классов (типа `TyCon`). Далее эти определения передаются в функцию, разрешающую констрейнты.

Разрешение констрейнтов происходит в функции 
```haskell ignore
solveDivRat :: RatDefs
			-> [Ct] -- given constraints
			-> [Ct] -- derived constraints
			-> [Ct] -- wanted constraints
			->  TcPluginM  TcPluginResult
```
`RatDefs` -- рекорд для определений, первый список `Ct` (сокращение для constraint) -- те констрейнты, которые выведены на данный момент, последний список `Ct` -- те, которые нужно вывести. 
Нужно заметить, что `TcPluginM` -- монада, в которой происходят все вычисления, а `TcPluginResult` это дата класс
```haskell ignore
data TcPluginResult = TcPluginContradiction [Ct]
					| TcPluginOk [(EvTerm, Ct)] [Ct]
```
То есть возвращается либо информация о том, что не удалось вывести констрейнт (например, недостаточно информации) `TcPluginContradiction [Ct]` (где в листе содержатся констрейнты, для которых не удалось сделать вывод), либо `TcPluginOk [(EvTerm, Ct)] [Ct]` (где в первом листе `[(EvTerm, Ct)]` содержится пара из констрейнта и доказательства, evidence для него, во втором содержатся новые wanteds констрейнты для плагина). Если плагину  не удалось сделать прогресс, должно быть возвращено `TcPluginOk [] []`.

Итак в этой функции изначально я фильтрую все wanteds констрейнты, полагаясь на то, что все необходимые для разрешения этим плагином имеют класс `KnownRat`.  Также я фильтрую givens констрейнты, так как только они нужны для разрешения констрейнтов (по задаче, если `KnownRat a, KnownRat b`, то компилятор сам может вывести  `KnownRat (DivRat a b)`, то есть для разрешения задачи нужно проверить, что для `DivRat a b` обе type variable имеют констрейнт `KnownRat`). Далее для каждого констрейнта я запускаю функцию 
```haskell ignore
constraintToEvTerm ::  RatDefs -- nesessary definitions
				   -> [Ct] -- the list of given constraints
				   ->  KrConstraint  -- current constraint we want to check
				   ->  Either  Ct ([(EvTerm, Ct)], [Ct])
```
Тип возвращаемой функции `Either`: `Left ct` означает, что данный констрейнт не может быть разрешен, `Right ([(EvTerm, Ct)], [Ct])` просто содержит в себе аутпут, который нужен для `TcPluginOk`. После чего я фильтрую список результатов для каждого констрейнта на lefts и rights, если есть хотя бы один Left, возвращаю `TcPluginContradiction lefts`, иначе проверяю, что список rights непустой (если он пуст, значит, плагин не смог сделать прогресса), и возвращаю `TcPluginOk`.

Внутри самой функции `constraintToEvTerm` я смотрю сначала на type constructor для типа, для которого проверяется констрейнт: для `DivRat` он должен быть `TyConApp TyCon [Type] -- Type constructor application`. Если он не такой, или же класс `tyCon` не совпадает с `DivRat`, то возвращается `Right ([], [ct])`: плагин не является разрешателем этих констрейнтов. 
Теперь мы знаем, что мы пытаемся решить `KnownRat (DivRat ..)`. У тайп конструктора для `DivRat` список `[Type]` -- это аргументы для `DivRat`, то есть type variables (на данный момент плагин решает только для случая `DivRat a b`). И для каждой такой переменной мы можем проверить, что она есть в списке данных констрейнтов. Попробуем найти для каждой переменной ее `EvTerm` с помощью функции `checkKnownRatConstraint`. Если хотя бы для одной переменной не было найдено соответствующего доказательства, то возвращается `Left ct` (то есть констрейнт неразрешим). Иначе воспользуемся хаком, описанным в статье [Solving GHCs KnownNat constraints](https://qbaylogic.com/blog/2016/08/10/solving-knownnat-constraints-plugin.html). Пока что мне непонятно, как это работает, работает ли вообще, и почему в коде, на который он ссылается, все так сложно, но, надеюсь, в относительно скором времени здесь появится мое описание происходящего.