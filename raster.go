package wktraster

import (
	"bytes"
	"encoding/binary"
	"encoding/hex"
	"errors"
	"io"
	"math"
)

//
// +---------------+-------------+------------------------------+
// | version       | uint16      | format version (0 for this   |
// |               |             | structure)                   |
// +---------------+-------------+------------------------------+
// | nBands        | uint16      | Number of bands              |
// +---------------+-------------+------------------------------+
// | scaleX        | float64     | pixel width                  |
// |               |             | in geographical units        |
// +---------------+-------------+------------------------------+
// | scaleY        | float64     | pixel height                 |
// |               |             | in geographical units        |
// +---------------+-------------+------------------------------+
// | ipX           | float64     | X ordinate of upper-left     |
// |               |             | pixel's upper-left corner    |
// |               |             | in geographical units        |
// +---------------+-------------+------------------------------+
// | ipY           | float64     | Y ordinate of upper-left     |
// |               |             | pixel's upper-left corner    |
// |               |             | in geographical units        |
// +---------------+-------------+------------------------------+
// | skewX         | float64     | rotation about Y-axis        |
// +---------------+-------------+------------------------------+
// | skewY         | float64     | rotation about X-axis        |
// +---------------+-------------+------------------------------+
// | srid          | int32       | Spatial reference id         |
// +---------------+-------------+------------------------------+
// | width         | uint16      | number of pixel columns      |
// +---------------+-------------+------------------------------+
// | height        | uint16      | number of pixel rows         |
// +---------------+-------------+------------------------------+

// +---------------+--------------+-----------------------------------+
// | isOffline     | 1bit         | If true, data is to be found      |
// |               |              | on the filesystem, trought the    |
// |               |              | path specified in RASTERDATA      |
// +---------------+--------------+-----------------------------------+
// | hasNodataValue| 1bit         | If true, stored nodata value is   |
// |               |              | a true nodata value. Otherwise    |
// |               |              | the value stored as a nodata      |
// |               |              | value should be ignored.          |
// +---------------+--------------+-----------------------------------+
// | isNodataValue | 1bit         | If true, all the values of the    |
// |               |              | band are expected to be nodata    |
// |               |              | values. This is a dirty flag.     |
// |               |              | To set the flag to its real value |
// |               |              | the function st_bandisnodata must |
// |               |              | must be called for the band with  |
// |               |              | 'TRUE' as last argument.          |
// +---------------+--------------+-----------------------------------+
// | reserved      | 1bit         | unused in this version            |
// +---------------+--------------+-----------------------------------+
// | pixtype       | 4bits        | 0: 1-bit boolean                  |
// |               |              | 1: 2-bit unsigned integer         |
// |               |              | 2: 4-bit unsigned integer         |
// |               |              | 3: 8-bit signed integer           |
// |               |              | 4: 8-bit unsigned integer         |
// |               |              | 5: 16-bit signed integer          |
// |               |              | 6: 16-bit unsigned signed integer |
// |               |              | 7: 32-bit signed integer          |
// |               |              | 8: 32-bit unsigned signed integer |
// |               |              | 9: 32-bit float                   |
// |               |              | 10: 64-bit float                  |
// +---------------+--------------+-----------------------------------+

// +---------------+--------------+-----------------------------------+
// | nodata        | 1 to 8 bytes | Nodata value                      |
// |               | depending on |                                   |
// |               | pixtype [1]  |                                   |
// +---------------+--------------+-----------------------------------+

// +---------------+--------------+-----------------------------------+
// | pix[w*h]      | 1 to 8 bytes | Pixels values, row after row,     |
// |               | depending on | so pix[0] is upper-left, pix[w-1] |
// |               | pixtype [1]  | is upper-right.                   |
// |               |              |                                   |
// |               |              | As for endiannes, it is specified |
// |               |              | at the start of WKB, and implicit |
// |               |              | up to 8bits (bit-order is most    |
// |               |              | significant first)                |
// |               |              |                                   |
// +---------------+--------------+-----------------------------------+

const (
	NDR     = 1
	XDR     = 0
	VERSION = 0
)

type DataType int

const (
	DT_Byte    = DataType(0)
	DT_Int16   = DataType(1)
	DT_UInt16  = DataType(2)
	DT_Int32   = DataType(3)
	DT_UInt32  = DataType(4)
	DT_Float32 = DataType(5)
	DT_Float64 = DataType(6)
)

var (
	PixTypes = map[DataType]int{
		DT_Byte:    4,
		DT_Int16:   5,
		DT_UInt16:  6,
		DT_Int32:   7,
		DT_UInt32:  8,
		DT_Float32: 10,
		DT_Float64: 11,
	}
)

type WKBHeader struct {
	Version uint16
	Bands   uint16
	ScaleX  float64
	ScaleY  float64
	IpX     float64
	IpY     float64
	SkewX   float64
	SkewY   float64
	Srid    int32
	Width   uint16
	Height  uint16
}

type WKBRasterPixel struct {
	Data interface{}
}

func (p *WKBRasterPixel) IsNil() bool {
	return p.Data == nil
}

func (p *WKBRasterPixel) GetData() interface{} {
	return p.Data
}

func (p *WKBRasterPixel) IsInt8() bool {
	switch p.Data.(type) {
	case int8:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsUInt8() bool {
	switch p.Data.(type) {
	case uint8:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsInt16() bool {
	switch p.Data.(type) {
	case int16:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsUInt16() bool {
	switch p.Data.(type) {
	case uint16:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsInt32() bool {
	switch p.Data.(type) {
	case int32:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsUInt32() bool {
	switch p.Data.(type) {
	case uint32:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsFloat32() bool {
	switch p.Data.(type) {
	case float32:
		return true
	}
	return false
}

func (p *WKBRasterPixel) IsFloat64() bool {
	switch p.Data.(type) {
	case float64:
		return true
	}
	return false
}

func (p *WKBRasterPixel) GetDataType() DataType {
	switch p.Data.(type) {
	case int8:
		return DT_Byte
	case uint8:
		return DT_Byte
	case int16:
		return DT_Int16
	case uint16:
		return DT_UInt16
	case int32:
		return DT_Int32
	case uint32:
		return DT_UInt32
	case float32:
		return DT_Float32
	case float64:
		return DT_Float64
	}
	return DT_Byte
}

type WKBRasterBand struct {
	NoData         WKBRasterPixel
	IsOffline      bool
	HasNoDataValue bool
	IsNoDataValue  bool
	Data           [][]WKBRasterPixel
}

type WKBRaster struct {
	Version uint16
	ScaleX  float64
	ScaleY  float64
	IpX     float64
	IpY     float64
	SkewX   float64
	SkewY   float64
	Srid    int32
	Width   uint16
	Height  uint16
	Bands   []WKBRasterBand
}

type Raster interface {
	GetRasterCount() int
	GetRasterBand(b int) RasterBand
	GetSize() []int
	GetSizeX() *int
	GetSizeY() *int
	GetTransform() []float64
}

type RasterBand interface {
	GetSize() []int
	ReadAsArray(xoff, yoff int, size []int, blockSize []int) interface{}
	GetNoDataValue() interface{}
	GetDataType() DataType
}

type RasterOptions struct {
	BlockSize     []int
	Endian        int
	Version       int
	Band          int
	SRID          int
	OverviewLevel int
}

type RasterTile struct {
	Level       int
	NoDataValue []interface{}
	Extent      [][]float64
	PixelSize   []int
	Blocksize   []int
	PixelTypes  []DataType
	SRID        int
	Data        []RasterTileData
}

type RasterTileData struct {
	Data []byte
}

func (d *RasterTileData) Hex() string {
	return hex.EncodeToString(d.Data)
}

func ConvertPixelData(pixels interface{}, size []int) (interface{}, error) {
	switch data := pixels.(type) {
	case []int8:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]int8, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]int8, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []uint8:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]uint8, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]uint8, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []int16:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]int16, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]int16, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []uint16:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]uint16, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]uint16, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []int32:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]int32, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]int32, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []uint32:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]uint32, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]uint32, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []float32:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]float32, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]float32, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case []float64:
		if len(data) != size[0]*size[1] {
			return nil, errors.New("dim error")
		}
		ret := make([][]float64, size[1])
		for r := 0; r < size[1]; r++ {
			ret[r] = make([]float64, size[0])
			for c := 0; c < size[0]; c++ {
				ret[r][c] = data[r*size[0]+c]
			}
		}
	case [][]int8:
		return data, nil
	case [][]uint8:
		return data, nil
	case [][]int16:
		return data, nil
	case [][]uint16:
		return data, nil
	case [][]int32:
		return data, nil
	case [][]uint32:
		return data, nil
	case [][]float32:
		return data, nil
	case [][]float64:
		return data, nil
	}
	return 0, errors.New("err dim")
}

func calculateGridSize(rasterSize, blockSize []int) []int {
	nx := rasterSize[0] / blockSize[0]
	ny := rasterSize[1] / blockSize[1]
	return []int{int(math.Ceil(float64(nx))), int(math.Ceil(float64(ny)))}
}

func calculateBlockPadSize(band RasterBand, xoff, yoff int, blockSize []int) []int {
	xpad := 0
	ypad := 0
	blockBound := [2]int{xoff + blockSize[0], yoff + blockSize[1]}

	if blockBound[0] > band.GetSize()[0] {
		xpad = blockBound[0] - band.GetSize()[0]
	}
	if blockBound[1] > band.GetSize()[1] {
		ypad = blockBound[1] - band.GetSize()[1]
	}

	return []int{xpad, ypad}
}

func calculateGeoxy(gt []float64, xy []float64) []float64 {
	xgeo := gt[0] + gt[1]*xy[0] + gt[2]*xy[1]
	ygeo := gt[3] + gt[4]*xy[0] + gt[5]*xy[1]
	return []float64{xgeo, ygeo}
}

func calculateGeoxyLevel(gt []float64, xy []float64, level int) []float64 {
	newgt := []float64{gt[0], gt[1] * float64(level), gt[2], gt[3], gt[4], gt[5] * float64(level)}
	return calculateGeoxy(newgt, xy)
}

func calculateBoundingBox(ds Raster, gt []float64) [][]float64 {
	dim := [][]float64{
		[]float64{0, 0},
		[]float64{0, float64(ds.GetSize()[1])},
		[]float64{float64(ds.GetSize()[0]), 0},
		[]float64{float64(ds.GetSize()[0]), float64(ds.GetSize()[1])},
	}

	ext := [][]float64{
		calculateGeoxy(gt, dim[0]),
		calculateGeoxy(gt, dim[1]),
		calculateGeoxy(gt, dim[2]),
		calculateGeoxy(gt, dim[3]),
	}

	return ext
}

func writeRasterHeader(write io.Writer, options RasterOptions, ds Raster, level int, ulp [2]int, xsize, ysize *int) error {
	if level < 1 {
		return errors.New("Error: Level must >= 1")
	}
	if xsize == nil || ysize == nil {
		xsize = ds.GetSizeX()
		ysize = ds.GetSizeY()
	}

	gt := ds.GetTransform()
	ul := calculateGeoxy(gt, []float64{float64(ulp[0]), float64(ulp[1])})
	rtIP := []float64{ul[0], ul[1]}
	rtSkew := []float64{gt[2], gt[4]}
	rtScale := []float64{gt[1] * float64(level), gt[5] * float64(level)}

	var header WKBHeader

	_, err := write.Write([]byte{byte(options.Endian)})
	if err != nil {
		return err
	}
	header.Version = uint16(options.Version)
	header.Bands = uint16(options.Band)
	header.ScaleX = rtScale[0]
	header.ScaleY = rtScale[1]
	header.IpX = rtIP[0]
	header.IpY = rtIP[1]
	header.SkewX = rtSkew[0]
	header.SkewY = rtSkew[1]
	header.Srid = int32(options.SRID)

	header.Width = uint16(*xsize)
	header.Height = uint16(*ysize)

	var endiannes binary.ByteOrder

	if options.Endian == 0 {
		endiannes = binary.BigEndian
	} else if options.Endian == 1 {
		endiannes = binary.LittleEndian
	}

	err = binary.Write(write, endiannes, header)
	if err != nil {
		return err
	}
	return nil
}

func writeRasterBandHeader(write io.Writer, options RasterOptions, band RasterBand) error {
	first4bits := 0

	var endiannes binary.ByteOrder

	if options.Endian == 0 {
		endiannes = binary.BigEndian
	} else if options.Endian == 1 {
		endiannes = binary.LittleEndian
	}

	nodata := band.GetNoDataValue()
	if nodata != nil {
		first4bits += 64
	} else {
		nodata = 0
	}
	pixtype := PixTypes[band.GetDataType()]
	_, err := write.Write([]byte{byte(pixtype + first4bits)})
	if err != nil {
		return err
	}
	var data []byte
	switch t := nodata.(type) {
	case int8:
		data = []byte{byte(t)}
	case uint8:
		data = []byte{t}
	case int16:
		endiannes.PutUint16(data, uint16(t))
	case uint16:
		endiannes.PutUint16(data, t)
	case int32:
		endiannes.PutUint32(data, uint32(t))
	case uint32:
		endiannes.PutUint32(data, t)
	case float32:
		bs := math.Float32bits(t)
		endiannes.PutUint32(data, bs)
	case float64:
		bs := math.Float64bits(t)
		endiannes.PutUint64(data, bs)
	}
	_, err = write.Write(data)
	if err != nil {
		return err
	}
	return nil
}

func makePixels(len int, nodata WKBRasterPixel) []WKBRasterPixel {
	ret := make([]WKBRasterPixel, len)
	for i := range ret {
		ret[i] = nodata
	}
	return ret
}

func writeIntOfType(writer io.Writer, endiannes binary.ByteOrder, valueType WKBRasterPixel) error {
	switch t := valueType.Data.(type) {
	case int8:
		_, err := writer.Write([]byte{byte(t)})
		return err
	case uint8:
		_, err := writer.Write([]byte{t})
		return err
	case int16:
		return binary.Write(writer, endiannes, t)
	case uint16:
		return binary.Write(writer, endiannes, t)
	case int32:
		return binary.Write(writer, endiannes, t)
	case uint32:
		return binary.Write(writer, endiannes, t)
	case float32:
		return binary.Write(writer, endiannes, t)
	case float64:
		return binary.Write(writer, endiannes, t)
	}
	return nil
}

func getPixelData(pixels interface{}, r, c int) interface{} {
	switch data := pixels.(type) {
	case [][]int8:
		return data[r][c]
	case [][]uint8:
		return data[r][c]
	case [][]int16:
		return data[r][c]
	case [][]uint16:
		return data[r][c]
	case [][]int32:
		return data[r][c]
	case [][]uint32:
		return data[r][c]
	case [][]float32:
		return data[r][c]
	case [][]float64:
		return data[r][c]
	default:
		return 0
	}
}

func writeRasterBand(write io.Writer, options RasterOptions, band RasterBand, level, xoff, yoff int, readBlockSize, blockSize []int) error {
	readPaddingSize := calculateBlockPadSize(band, xoff, yoff, readBlockSize)
	validReadBlockSize := []int{readBlockSize[0] - readPaddingSize[0],
		readBlockSize[1] - readPaddingSize[1]}

	var targetBlockSize []int
	var targetPaddingSize []int
	if readPaddingSize[0] > 0 || readPaddingSize[1] > 0 {
		targetBlockSize = []int{validReadBlockSize[0] / level, validReadBlockSize[1] / level}
		targetPaddingSize = []int{readPaddingSize[0] / level, readPaddingSize[1] / level}
	} else {
		targetBlockSize = blockSize
		targetPaddingSize = []int{0, 0}
	}

	if validReadBlockSize[0] <= 0 || validReadBlockSize[1] <= 0 {
		return errors.New("ReadBlockSize must > 0")
	}
	if targetBlockSize[0] <= 0 || targetBlockSize[1] <= 0 {
		return errors.New("TargetBlockSize must > 0")
	}

	outPixels := make([][]WKBRasterPixel, blockSize[0])
	pixels := band.ReadAsArray(xoff, yoff, validReadBlockSize, targetBlockSize)
	var err error
	pixels, err = ConvertPixelData(pixels, validReadBlockSize)
	if err != nil {
		return err
	}

	pixeldYLen, pixeldXLen := func() (int, int) {
		switch data := pixels.(type) {
		case [][]int8:
			return len(data), len(data[0])
		case [][]uint8:
			return len(data), len(data[0])
		case [][]int16:
			return len(data), len(data[0])
		case [][]uint16:
			return len(data), len(data[0])
		case [][]int32:
			return len(data), len(data[0])
		case [][]uint32:
			return len(data), len(data[0])
		case [][]float32:
			return len(data), len(data[0])
		case [][]float64:
			return len(data), len(data[0])
		default:
			return 0, 0
		}
	}()

	if targetPaddingSize[0] > 0 || targetPaddingSize[1] > 0 {
		ysizeReadPixels := pixeldYLen
		nodataValue := WKBRasterPixel{Data: band.GetNoDataValue()}

		padCols := makePixels(targetPaddingSize[0], nodataValue)
		for row := 0; row < ysizeReadPixels; row++ {
			outLine := make([]WKBRasterPixel, pixeldXLen)
			for i := 0; i < pixeldXLen; i++ {
				outLine[i] = WKBRasterPixel{Data: getPixelData(pixels, row, i)}
			}
			outLine = append(outLine, padCols...)
			outPixels[row] = outLine
		}

		for row := ysizeReadPixels; row < ysizeReadPixels+targetPaddingSize[1]; row++ {
			outLine := makePixels(pixeldXLen, nodataValue)
			outPixels[row] = outLine
		}
	} else {
		for r := 0; r < blockSize[0]; r++ {
			outPixels[r] = make([]WKBRasterPixel, blockSize[1])
			for c := 0; c < blockSize[1]; c++ {
				outPixels[r][c] = WKBRasterPixel{Data: getPixelData(pixels, r, c)}
			}
		}
	}

	var endiannes binary.ByteOrder

	if options.Endian == 0 {
		endiannes = binary.BigEndian
	} else if options.Endian == 1 {
		endiannes = binary.LittleEndian
	}

	for r := 0; r < blockSize[0]; r++ {
		for c := 0; c < blockSize[0]; c++ {
			writeIntOfType(write, endiannes, outPixels[r][c])
		}
	}

	return nil
}

func collectPixelTypes(ds Raster, bandFrom, bandTo int) []DataType {
	var pt []DataType

	for i := bandFrom; i < bandTo; i++ {
		band := ds.GetRasterBand(i)
		pt = append(pt, band.GetDataType())
	}
	return pt
}

func collectNodataValues(ds Raster, bandFrom, bandTo int) []interface{} {
	var nd []interface{}

	for i := bandFrom; i < bandTo; i++ {
		band := ds.GetRasterBand(i)
		nodata := band.GetNoDataValue()
		if nodata != nil {
			nd = append(nd, nodata)
		}
	}
	return nd
}

func writeRasterLevel(options RasterOptions, ds Raster, level int, bandRange []int) (*RasterTile, error) {
	bandFrom := bandRange[0]
	bandTo := bandRange[1]

	var blockSize []int
	var readBlockSize []int
	var gridSize []int

	rasterSize := ds.GetSize()
	if options.BlockSize == nil {
		blockSize = []int{options.BlockSize[0], options.BlockSize[1]}
		readBlockSize = []int{blockSize[0] * level, blockSize[1] * level}
		gridSize = calculateGridSize(rasterSize, readBlockSize)
	} else {
		blockSize = rasterSize
		readBlockSize = blockSize
		gridSize = []int{1, 1}
	}

	pixelSize := []int{int(ds.GetTransform()[1]), int(ds.GetTransform()[5])}
	pixelTypes := collectPixelTypes(ds, bandFrom, bandTo)
	nodataValues := collectNodataValues(ds, bandFrom, bandTo)
	extent := calculateBoundingBox(ds, ds.GetTransform())

	tile := RasterTile{Level: level,
		NoDataValue: nodataValues,
		PixelSize:   pixelSize,
		PixelTypes:  pixelTypes,
		Blocksize:   blockSize,
		Extent:      extent,
		SRID:        options.SRID,
	}

	var datas []RasterTileData

	for ycell := 0; ycell < gridSize[1]; ycell++ {
		for xcell := 0; xcell < gridSize[0]; xcell++ {

			buffer := new(bytes.Buffer)

			xoff := xcell * readBlockSize[0]
			yoff := ycell * readBlockSize[1]

			if options.BlockSize != nil {
				err := writeRasterHeader(buffer, options, ds, level, [2]int{xoff, yoff},
					&blockSize[0], &blockSize[1])
				if err != nil {
					return nil, err
				}
			} else {
				err := writeRasterHeader(buffer, options, ds, level, [2]int{xoff, yoff}, nil, nil)
				if err != nil {
					return nil, err
				}
			}

			for b := bandFrom; b < bandTo; b++ {
				band := ds.GetRasterBand(b)
				err := writeRasterBandHeader(buffer, options, band)
				if err != nil {
					return nil, err
				}
				err = writeRasterBand(buffer, options, band, level, xoff, yoff, readBlockSize, blockSize)
				if err != nil {
					return nil, err
				}
			}

			datas = append(datas, RasterTileData{Data: buffer.Bytes()})
		}
	}

	tile.Data = datas

	return &tile, nil
}

func WriteWKBRaster(options RasterOptions, ds Raster) (*RasterTile, error) {
	var bandRange []int
	if options.Band > 0 {
		bandRange = []int{options.Band, options.Band + 1}
	} else {
		bandRange = []int{1, ds.GetRasterCount() + 1}
	}

	return writeRasterLevel(options, ds, options.OverviewLevel, bandRange)
}

func readIntOfType(reader io.Reader, endiannes binary.ByteOrder, valueType int) (WKBRasterPixel, error) {
	switch valueType {
	case 0, 1, 2, 4:
		var value uint8

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 3:
		var value int8

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 5:
		var value int16

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 6:
		var value uint16

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 7:
		var value int32

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 8:
		var value uint32

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 9:
		var value float32

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	case 10:
		var value float64

		err := binary.Read(reader, endiannes, &value)

		if err != nil {
			return WKBRasterPixel{Data: 0}, err
		}

		return WKBRasterPixel{Data: value}, nil
	}

	return WKBRasterPixel{Data: 0}, errors.New("Unknown value type")
}

func ReadWKBRaster(wkb io.Reader) (WKBRaster, error) {
	raster := WKBRaster{}

	var endiannesValue uint8
	err := binary.Read(wkb, binary.LittleEndian, &endiannesValue)

	if err != nil {
		return raster, err
	}

	var endiannes binary.ByteOrder

	if endiannesValue == 0 {
		endiannes = binary.BigEndian
	} else if endiannesValue == 1 {
		endiannes = binary.LittleEndian
	}

	var header WKBHeader
	err = binary.Read(wkb, endiannes, &header)

	if err != nil {
		return raster, err
	}

	raster.Version = header.Version
	raster.ScaleX = header.ScaleX
	raster.ScaleY = header.ScaleY
	raster.IpX = header.IpX
	raster.IpY = header.IpY
	raster.SkewX = header.SkewX
	raster.SkewY = header.SkewY
	raster.Srid = header.Srid
	raster.Width = header.Width
	raster.Height = header.Height

	for i := 0; i < int(header.Bands); i++ {
		band := WKBRasterBand{}

		var bandheader uint8
		err := binary.Read(wkb, endiannes, &bandheader)

		if err != nil {
			return raster, err
		}

		band.IsOffline = (int(bandheader) & 128) != 0
		band.HasNoDataValue = (int(bandheader) & 64) != 0
		band.IsNoDataValue = (int(bandheader) & 32) != 0

		pixType := (int(bandheader) & 15) - 1

		noData, err := readIntOfType(wkb, endiannes, pixType)

		if err != nil {
			return raster, err
		}

		band.NoData = noData

		for r := 0; r < int(header.Height); r++ {
			row := []WKBRasterPixel{}

			for c := 0; c < int(header.Width); c++ {
				value, err := readIntOfType(wkb, endiannes, pixType)

				if err != nil {
					return raster, err
				}

				row = append(row, value)
			}

			band.Data = append(band.Data, row)
		}

		raster.Bands = append(raster.Bands, band)
	}

	return raster, nil
}
